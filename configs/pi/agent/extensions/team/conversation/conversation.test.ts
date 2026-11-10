import type { AssistantMessage, UserMessage } from "@earendil-works/pi-ai";
import { describe, expect, it } from "vitest";
import {
	Conversation,
	CONVERSATION_MAX_BYTES,
	CONVERSATION_MAX_FIELD_BYTES,
	CONVERSATION_MAX_ITEMS,
} from "./index";
import type { ActiveAgentSummary } from "../runtime";

const agent: ActiveAgentSummary = {
	id: "a",
	agentName: "worker",
	brief: "",
	status: "running",
	turns: 0,
	contextTokens: 0,
	model: undefined,
	startedAt: 0,
	result: undefined,
	error: undefined,
};
const assistant = (text: string): AssistantMessage => ({
	role: "assistant",
	content: [{ type: "text", text }],
	api: "openai-completions",
	provider: "openai",
	model: "test",
	usage: {
		input: 0,
		output: 0,
		cacheRead: 0,
		cacheWrite: 0,
		totalTokens: 0,
		cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
	},
	stopReason: "stop",
	timestamp: 1,
});

describe("conversation capture", () => {
	it("replaces streamed messages and deduplicates completion by identity, not text", () => {
		const conversation = new Conversation();
		conversation.prompt("task");
		const user: UserMessage = { role: "user", content: "task", timestamp: 0 };
		conversation.capture({ type: "message_start", message: user });
		conversation.capture({ type: "message_end", message: user });
		conversation.capture({ type: "message_start", message: assistant("partial") });
		const update = assistant("streamed");
		conversation.capture({
			type: "message_update",
			message: update,
			assistantMessageEvent: {
				type: "text_delta",
				contentIndex: 0,
				delta: "streamed",
				partial: update,
			},
		});
		expect(conversation.snapshot(agent).items[1]).toMatchObject({
			kind: "assistant",
			text: "streamed",
		});
		const final = assistant("same");
		conversation.capture({ type: "message_end", message: final });
		conversation.capture({ type: "message_end", message: final });
		const second = assistant("same");
		conversation.capture({ type: "message_start", message: second });
		conversation.capture({ type: "message_end", message: second });
		expect(
			conversation
				.snapshot(agent)
				.items.map((item) => (item.kind === "tool" ? item.name : item.text)),
		).toEqual(["task", "same", "same"]);
	});

	it("replaces transformed prompt echoes by identity and clears unacknowledged cycles", () => {
		const conversation = new Conversation();
		conversation.prompt("/expanded");
		conversation.capture({
			type: "message_start",
			message: { role: "user", content: "expanded prompt", timestamp: 1 },
		});
		conversation.prompt("followup");
		conversation.capture({
			type: "message_start",
			message: { role: "user", content: "expanded followup", timestamp: 2 },
		});
		conversation.prompt("/no-echo");
		conversation.endPromptCycle();
		conversation.capture({
			type: "message_start",
			message: { role: "user", content: "unrelated", timestamp: 3 },
		});
		expect(conversation.snapshot(agent).items).toEqual([
			expect.objectContaining({ text: "expanded prompt" }),
			expect.objectContaining({ text: "expanded followup" }),
			expect.objectContaining({ text: "/no-echo" }),
			expect.objectContaining({ text: "unrelated" }),
		]);
	});

	it("defers streamed arguments while retaining text, final arguments and partial output", () => {
		const conversation = new Conversation();
		const message = assistant("partial");
		const serialize = new Proxy(
			{ data: "partial" },
			{
				ownKeys: () => {
					throw new Error("serialized prematurely");
				},
			},
		);
		message.content.push({ type: "toolCall", id: "call", name: "read", arguments: serialize });
		conversation.capture({ type: "message_start", message });
		conversation.capture({
			type: "message_update",
			message,
			assistantMessageEvent: {
				type: "text_delta",
				contentIndex: 0,
				delta: "partial",
				partial: message,
			},
		});
		expect(conversation.snapshot(agent).items).toEqual([
			expect.objectContaining({ text: "partial" }),
			expect.objectContaining({ arguments: "" }),
		]);
		conversation.capture({
			type: "message_update",
			message,
			assistantMessageEvent: {
				type: "toolcall_end",
				contentIndex: 1,
				toolCall: { type: "toolCall", id: "call", name: "read", arguments: { path: "finished" } },
				partial: message,
			},
		});
		expect(conversation.snapshot(agent).items[1]).toMatchObject({
			arguments: '{"path":"finished"}',
		});
		conversation.capture({
			type: "tool_execution_start",
			toolCallId: "call",
			toolName: "read",
			args: { path: "complete" },
		});
		conversation.capture({
			type: "tool_execution_update",
			toolCallId: "call",
			toolName: "read",
			args: serialize,
			partialResult: { content: [{ type: "text", text: "output" }] },
		});
		expect(conversation.snapshot(agent).items[1]).toMatchObject({
			arguments: '{"path":"complete"}',
			output: "output",
			partial: true,
		});
	});

	it("keeps a deterministic compact tool item when escaped fields exceed the transcript budget", () => {
		const conversation = new Conversation();
		conversation.prompt("newest prompt");
		const control = "\u0001".repeat(CONVERSATION_MAX_FIELD_BYTES);
		conversation.capture({
			type: "tool_execution_start",
			toolCallId: control,
			toolName: control,
			args: { data: control },
		});
		conversation.capture({
			type: "tool_execution_end",
			toolCallId: control,
			toolName: control,
			result: { content: [{ type: "text", text: control }] },
			isError: false,
		});
		const snapshot = conversation.snapshot(agent);
		expect(snapshot.items).toHaveLength(2);
		expect(snapshot.items[1]).toMatchObject({
			kind: "tool",
			name: "[truncated]",
			arguments: "[truncated]",
			output: "[truncated]",
		});
		expect(snapshot.truncated).toBe(true);
		expect(Buffer.byteLength(JSON.stringify(snapshot.items))).toBeLessThanOrEqual(
			CONVERSATION_MAX_BYTES,
		);
	});

	it("replaces tool partials with authoritative results and omits image data and signatures", () => {
		const conversation = new Conversation();
		const message = assistant("working");
		message.content.push({
			type: "toolCall",
			id: "call",
			name: "read",
			arguments: { path: "file", image: { type: "image", data: "SECRET" }, signature: "SECRET" },
		});
		conversation.capture({ type: "message_start", message });
		conversation.capture({ type: "message_end", message });
		conversation.capture({
			type: "tool_execution_start",
			toolCallId: "call",
			toolName: "read",
			args: { path: "file" },
		});
		conversation.capture({
			type: "tool_execution_update",
			toolCallId: "call",
			toolName: "read",
			args: { path: "file" },
			partialResult: { content: [{ type: "text", text: "partial" }] },
		});
		conversation.capture({
			type: "tool_execution_end",
			toolCallId: "call",
			toolName: "read",
			result: {
				content: [
					{ type: "text", text: "final" },
					{ type: "image", data: "SECRET", mimeType: "image/png" },
				],
			},
			isError: true,
		});
		conversation.capture({
			type: "tool_execution_update",
			toolCallId: "call",
			toolName: "read",
			args: { path: "file" },
			partialResult: { content: [{ type: "text", text: "late partial" }] },
		});
		const snapshot = conversation.snapshot(agent);
		expect(snapshot.items.filter((item) => item.kind === "tool")).toEqual([
			{
				kind: "tool",
				id: "call",
				name: "read",
				arguments: '{"path":"file"}',
				output: "final\n[image omitted]",
				isError: true,
				partial: false,
			},
		]);
		expect(JSON.stringify(snapshot)).not.toContain("SECRET");
		const result = {
			role: "toolResult" as const,
			toolCallId: "call",
			toolName: "read",
			content: [{ type: "text" as const, text: "authoritative" }],
			isError: false,
			timestamp: 1,
		};
		conversation.capture({ type: "message_start", message: result });
		conversation.capture({ type: "message_end", message: result });
		expect(conversation.snapshot(agent).items.filter((item) => item.kind === "tool")).toEqual([
			expect.objectContaining({ output: "authoritative", isError: false }),
		]);
	});

	it("enforces UTF-8 field, item and total byte limits", () => {
		const conversation = new Conversation();
		conversation.prompt("😀".repeat(20_000));
		let snapshot = conversation.snapshot(agent);
		const first = snapshot.items[0];
		if (!first || first.kind !== "user") throw new Error("missing user");
		expect(Buffer.byteLength(first.text)).toBeLessThanOrEqual(CONVERSATION_MAX_FIELD_BYTES);
		expect(first.text).toContain("[truncated]");
		for (let i = 0; i < 220; i++) conversation.prompt(`${i}`.repeat(5000));
		snapshot = conversation.snapshot(agent);
		expect(snapshot.truncated).toBe(true);
		expect(snapshot.items.length).toBeLessThanOrEqual(CONVERSATION_MAX_ITEMS);
		expect(Buffer.byteLength(JSON.stringify(snapshot.items))).toBeLessThanOrEqual(
			CONVERSATION_MAX_BYTES,
		);
		const itemLimited = new Conversation();
		for (let i = 0; i < 205; i++) itemLimited.prompt(String(i));
		expect(itemLimited.snapshot(agent).items).toHaveLength(CONVERSATION_MAX_ITEMS);
		expect(itemLimited.snapshot(agent).truncated).toBe(true);
	});
});
