/* oxlint-disable no-control-regex -- Assert terminal control safety. */
import { initTheme } from "@earendil-works/pi-coding-agent";
import {
	KeybindingsManager,
	Markdown,
	TUI_KEYBINDINGS,
	getCapabilities,
	setCapabilities,
	visibleWidth,
} from "@earendil-works/pi-tui";
import { beforeAll, describe, expect, it, vi } from "vitest";
import type { ConversationSnapshot } from "../../conversation";
import { createFleetOverlay } from "../overlay";
import { createConversationOverlay } from "./overlay";
import { sanitizeConversationText } from "./presentation";
import { createConversationState, moveConversation, positionConversation } from "./state";

const snapshot = (): ConversationSnapshot => ({
	agent: {
		id: "one",
		agentName: "agent",
		brief: "brief",
		status: "running",
		turns: 0,
		contextTokens: 0,
		startedAt: 0,
		model: undefined,
		result: undefined,
		error: undefined,
	},
	revision: 1,
	items: [
		{ kind: "user", id: "1", text: Array.from({ length: 12 }, (_, i) => `line ${i}`).join("\n") },
		{
			kind: "tool",
			id: "call",
			name: "read",
			arguments: "{}",
			output: "secret tool output",
			isError: true,
			partial: false,
		},
	],
	truncated: false,
});

beforeAll(() => initTheme("dark", false));

describe("conversation viewer", () => {
	it("renders genuine Markdown and falls back visibly on errors", () => {
		const current = snapshot();
		current.items = [
			{
				kind: "assistant",
				id: "md",
				text: "**bold**\n\n```js\nconst x = 1;\n```\n\n| A | B |\n|---|---|\n| one | two |\n\x1b]0;injected\x07safe",
			},
		];
		const overlay = createConversationOverlay(
			{ requestRender: () => {}, terminal: { rows: 100 } },
			new KeybindingsManager(TUI_KEYBINDINGS, {}),
			() => current,
			() => {},
		);
		const spy = vi.spyOn(Markdown.prototype, "render");
		try {
			const rendered = overlay.render(80).join("\n");
			expect(spy).toHaveBeenCalled();
			expect(rendered).toContain("bold");
			expect(rendered).not.toContain("**bold**");
			expect(rendered.replace(/\x1b\[[0-9;:]*m/g, "")).toContain("const x");
			expect(rendered).toContain("one");
			expect(rendered).not.toContain("[Markdown unavailable");
			spy.mockImplementation(() => {
				throw new Error("render failure");
			});
			overlay.invalidate();
			const fallback = overlay.render(80).join("\n");
			expect(fallback).toContain("[Markdown unavailable; showing raw text]");
			expect(fallback).toContain("**bold**");
			expect(fallback).not.toContain("\x1b]");
			expect(fallback).not.toContain("injected");
		} finally {
			spy.mockRestore();
		}
	});
	it("strips unsafe generated hyperlinks and bidi controls from actual output", () => {
		const capabilities = getCapabilities();
		setCapabilities({ ...capabilities, hyperlinks: true });
		const spy = vi.spyOn(Markdown.prototype, "render");
		try {
			for (const href of [
				"file:///etc/passwd",
				"javascript:alert(1)",
				"https://example.com/" + "a".repeat(120),
				"https://example.com/\x1b]0;injected\x07path",
				"https://example.com/\u202eevil",
			]) {
				const current = snapshot();
				current.items = [
					{
						kind: "assistant",
						id: "link",
						text: `[a long linked label that wraps over several lines](${href})`,
					},
				];
				current.agent.agentName = "agent\u202eevil";
				const overlay = createConversationOverlay(
					{ requestRender: () => {}, terminal: { rows: 100 } },
					new KeybindingsManager(TUI_KEYBINDINGS, {}),
					() => current,
					() => {},
				);
				const lines = overlay.render(20);
				expect(spy.mock.results.at(-1)?.value.join("\n")).toContain("\x1b]8;;");
				expect(lines.join("\n")).toContain("linked");
				expect(lines.join("\n")).not.toMatch(/\x1b\]|[\x90-\x9f\u202a-\u202e\u2066-\u2069]/);
				for (const line of lines) {
					expect(line.replace(/\x1b\[[0-9;:]*m/g, "")).not.toMatch(/[\x00-\x1f\x7f-\x9f]/);
					expect(visibleWidth(line)).toBeLessThanOrEqual(20);
				}
			}
		} finally {
			spy.mockRestore();
			setCapabilities(capabilities);
		}
	});
	it("rerenders only changed items and invalidates on options, width and theme", () => {
		const current = snapshot();
		current.items = Array.from({ length: 190 }, (_, i) => ({
			kind: "assistant" as const,
			id: String(i),
			text: `**item ${i}** ${"x".repeat(1290)}`,
		}));
		const overlay = createConversationOverlay(
			{ requestRender: () => {}, terminal: { rows: 30 } },
			new KeybindingsManager(TUI_KEYBINDINGS, {}),
			() => current,
			() => {},
		);
		const spy = vi.spyOn(Markdown.prototype, "render");
		try {
			overlay.render(80);
			expect(spy).toHaveBeenCalledTimes(190);
			current.items[189] = { kind: "assistant", id: "189", text: "updated" };
			current.revision++;
			overlay.render(80);
			expect(spy).toHaveBeenCalledTimes(191);
			overlay.render(60);
			expect(spy).toHaveBeenCalledTimes(381);
			overlay.invalidate();
			overlay.render(60);
			expect(spy).toHaveBeenCalledTimes(571);
			overlay.handleInput("m");
			overlay.render(60);
			overlay.handleInput("m");
			overlay.render(60);
			expect(spy).toHaveBeenCalledTimes(761);
		} finally {
			spy.mockRestore();
		}
	});
	it("handles unavailable capabilities and displays sanitized outcomes", () => {
		const current = snapshot();
		current.items = [];
		current.agent.error = "failed\x1b]0;evil\x07 safely";
		const overlay = createConversationOverlay(
			{ requestRender: () => {} },
			{
				matches: () => {
					throw new Error("bad keys");
				},
			},
			() => current,
			() => {},
		);
		expect(overlay.render(80).join("\n")).toContain("failed safely");
		current.agent.error = undefined;
		current.agent.result = "result text";
		expect(overlay.render(80).join("\n")).toContain("result text");
		const unavailable = createConversationOverlay(
			{ requestRender: () => {} },
			{ matches: () => false },
			() => {
				throw new Error("snapshot failed");
			},
			() => {},
		);
		unavailable.handleInput("j");
		expect(unavailable.render(80).join("\n")).toContain("unavailable or expired");
	});
	it("opens recent fleet rows using Enter", () => {
		const actions: string[] = [];
		const recent = { ...snapshot().agent, status: "done" as const, settledAt: 10 };
		const overlay = createFleetOverlay(
			{ requestRender: () => {} },
			new KeybindingsManager(TUI_KEYBINDINGS, {}),
			() => ({ rows: [{ kind: "recent", agent: recent }], detachedActiveCount: 0 }),
			(action) => actions.push(action.kind),
		);
		overlay.handleInput("\r");
		expect(actions).toEqual(["view"]);
	});
	it("strips escape and control strings while preserving line breaks", () => {
		expect(
			sanitizeConversationText(
				"a\x1b[31mred\x1b[0m\n\x1b]8;;evil\x07link\x1b]8;;\x1b\\!\x9dmalicious\x9cZ\x00",
			),
		).toBe("ared\nlink!Z");
	});
	it("handles bound, missing and throwing keys through the viewport", () => {
		const current = snapshot();
		current.items = [
			{
				kind: "user",
				id: "lines",
				text: Array.from({ length: 40 }, (_, i) => `line-${String(i).padStart(2, "0")}`).join("\n"),
			},
		];
		const keys = new KeybindingsManager(TUI_KEYBINDINGS, {
			"tui.select.up": "h",
			"tui.select.down": "l",
			"tui.select.pageUp": "b",
			"tui.select.pageDown": "f",
		});
		const overlay = createConversationOverlay(
			{ requestRender: () => {}, terminal: { rows: 12 } },
			keys,
			() => current,
			() => {},
		);
		overlay.handleInput("m");
		expect(overlay.render(80).join("\n")).toContain("line-39");
		overlay.handleInput("h");
		expect(overlay.render(80).join("\n")).toContain("line-34");
		overlay.handleInput("h");
		expect(overlay.render(80).join("\n")).not.toContain("line-39");
		overlay.handleInput("b");
		expect(overlay.render(80).join("\n")).toContain("line-27");
		overlay.handleInput("f");
		overlay.handleInput("l");
		overlay.handleInput("\x1b[H");
		expect(overlay.render(80).join("\n")).toContain("line-00");
		overlay.handleInput("\x1b[F");
		expect(overlay.render(80).join("\n")).toContain("line-39");
		for (const matches of [
			undefined,
			() => {
				throw new Error("bad binding");
			},
		]) {
			const fallback = createConversationOverlay(
				{ requestRender: () => {}, terminal: { rows: 12 } },
				{ matches } as unknown as typeof keys,
				() => current,
				() => {},
			);
			fallback.handleInput("m");
			fallback.render(80);
			fallback.handleInput("\x1b[H");
			expect(fallback.render(80).join("\n")).toContain("line-00");
			fallback.handleInput("\x1b[6~");
			expect(fallback.render(80).join("\n")).not.toContain("line-00");
		}
	});
	it("pauses following on scrolling and resumes at end", () => {
		let state = positionConversation(createConversationState(), 20, 5);
		expect(state.offset).toBe(15);
		state = moveConversation(state, "pageUp", 20, 5);
		expect(state.offset).toBe(10);
		expect(positionConversation(state, 30, 5).offset).toBe(10);
		state = moveConversation(state, "home", 30, 5);
		expect(state.offset).toBe(0);
		state = moveConversation(state, "end", 30, 5);
		expect(positionConversation(state, 40, 5).offset).toBe(35);
	});
	it("renders bounded lines, toggles tools and raw mode, refreshes on resize and reauthorizes", () => {
		let current: ConversationSnapshot | undefined = snapshot();
		let closed = false;
		const tui = { requestRender: () => {}, terminal: { rows: 30 } };
		const keys = new KeybindingsManager(TUI_KEYBINDINGS, {});
		const overlay = createConversationOverlay(
			tui,
			keys,
			() => current,
			() => {
				closed = true;
			},
		);
		expect(overlay.render(18).join("\n")).not.toContain("secret tool output");
		overlay.handleInput("t");
		overlay.handleInput("m");
		const lines = overlay.render(18);
		expect(lines.every((line) => visibleWidth(line) <= 18)).toBe(true);
		expect(lines.join("\n")).toContain("secret tool output");
		current = undefined;
		expect(overlay.render(60).join("\n")).toContain("unavailable or expired");
		overlay.handleInput("q");
		expect(closed).toBe(true);
	});
});
