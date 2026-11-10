import type { AgentSessionEvent } from "@earendil-works/pi-coding-agent";
import type { ActiveAgentSummary } from "../runtime";

export const CONVERSATION_MAX_ITEMS = 200;
export const CONVERSATION_MAX_BYTES = 256 * 1024;
export const CONVERSATION_MAX_FIELD_BYTES = 16 * 1024;
export const CONVERSATION_RECENT_PER_OWNER = 20;
export const CONVERSATION_RECENT_MAX_COUNT = 100;
export const CONVERSATION_RECENT_MAX_BYTES = 8 * 1024 * 1024;
export const CONVERSATION_RECENT_TTL_MS = 30_000;
export const CONVERSATION_TRUNCATED_LABEL = "\n[truncated]";

export type ConversationItem =
	| { kind: "user" | "assistant"; id: string; text: string }
	| {
			kind: "tool";
			id: string;
			name: string;
			arguments: string;
			output: string;
			isError: boolean;
			partial: boolean;
	  };

export type ConversationSnapshot = {
	agent: ActiveAgentSummary;
	revision: number;
	items: ConversationItem[];
	truncated: boolean;
	expiresAt?: number;
};

const bytes = (value: string): number => Buffer.byteLength(value, "utf8");

export function boundedText(value: string): { text: string; truncated: boolean } {
	if (bytes(value) <= CONVERSATION_MAX_FIELD_BYTES) return { text: value, truncated: false };
	const limit = CONVERSATION_MAX_FIELD_BYTES - bytes(CONVERSATION_TRUNCATED_LABEL);
	let low = 0;
	let high = Math.min(value.length, limit);
	while (low < high) {
		const middle = Math.ceil((low + high) / 2);
		if (bytes(value.slice(0, middle)) <= limit) low = middle;
		else high = middle - 1;
	}
	let end = low;
	if (end > 0 && /[\uD800-\uDBFF]/.test(value[end - 1] ?? "")) end--;
	return { text: value.slice(0, end) + CONVERSATION_TRUNCATED_LABEL, truncated: true };
}

const record = (value: unknown): value is Record<string, unknown> =>
	typeof value === "object" && value !== null;

function contentText(content: unknown): string {
	if (typeof content === "string") return content;
	if (!Array.isArray(content)) return "";
	return content
		.flatMap((part: unknown) => {
			if (!record(part)) return [];
			if (part.type === "text" && typeof part.text === "string") return [part.text];
			if (part.type === "image") return ["[image omitted]"];
			if (part.type === "thinking") return ["[thinking omitted]"];
			return [];
		})
		.join("\n");
}

function argumentsText(value: unknown): string {
	try {
		return (
			JSON.stringify(value, (key, entry: unknown) => {
				if (/signature/i.test(key)) return undefined;
				if (record(entry) && entry.type === "image") return "[image omitted]";
				return entry;
			}) ?? ""
		);
	} catch {
		return "[arguments unavailable]";
	}
}

export class Conversation {
	private items: ConversationItem[] = [];
	private nextId = 0;
	private assistantId: string | undefined;
	private pendingPromptId: string | undefined;
	private itemBytes = 2;
	private readonly itemSizes = new Map<
		string,
		{ size: number; fields: Map<string, { value: string; size: number }> }
	>();
	private readonly completedMessages: WeakRef<object>[] = [];
	private revision = 0;
	private truncated = false;

	prompt(text: string): void {
		const bounded = this.field(text);
		this.pendingPromptId = this.id();
		this.put({ kind: "user", id: this.pendingPromptId, text: bounded });
	}

	endPromptCycle(): void {
		this.pendingPromptId = undefined;
	}

	capture(event: AgentSessionEvent): boolean {
		const before = this.revision;
		if (
			event.type === "message_start" ||
			event.type === "message_update" ||
			event.type === "message_end"
		) {
			const message: unknown = event.message;
			if (!record(message)) return false;
			if (event.type === "message_end") {
				if (this.completedMessages.some((ref) => ref.deref() === message)) return false;
				this.completedMessages.push(new WeakRef(message));
				if (this.completedMessages.length > CONVERSATION_MAX_ITEMS) this.completedMessages.shift();
			}
			if (message.role === "user") {
				if (event.type === "message_start") {
					const text = this.field(contentText(message.content));
					this.put({ kind: "user", id: this.pendingPromptId ?? this.id(), text });
					this.pendingPromptId = undefined;
				}
			} else if (message.role === "assistant") {
				if (event.type === "message_start" || !this.assistantId) this.assistantId = this.id();
				const text = this.field(contentText(message.content));
				if (text || this.items.some((item) => item.id === this.assistantId)) {
					this.put({ kind: "assistant", id: this.assistantId, text });
				}
				if (Array.isArray(message.content)) {
					for (const [index, part] of message.content.entries()) {
						if (
							record(part) &&
							part.type === "toolCall" &&
							typeof part.id === "string" &&
							typeof part.name === "string"
						) {
							const completedCall =
								event.type === "message_update" &&
								event.assistantMessageEvent.type === "toolcall_end" &&
								event.assistantMessageEvent.contentIndex === index &&
								event.assistantMessageEvent.toolCall.id === part.id
									? event.assistantMessageEvent.toolCall
									: undefined;
							this.tool(
								part.id,
								part.name,
								completedCall?.arguments ??
									(event.type === "message_end" ? part.arguments : undefined),
							);
						}
					}
				}
				if (event.type === "message_end") this.assistantId = undefined;
			} else if (
				message.role === "toolResult" &&
				event.type === "message_end" &&
				typeof message.toolCallId === "string" &&
				typeof message.toolName === "string"
			) {
				this.tool(
					message.toolCallId,
					message.toolName,
					undefined,
					contentText(message.content),
					message.isError === true,
					false,
				);
			}
		} else if (
			(event.type === "tool_execution_start" || event.type === "tool_execution_update") &&
			typeof event.toolCallId === "string" &&
			typeof event.toolName === "string"
		) {
			this.tool(
				event.toolCallId,
				event.toolName,
				event.type === "tool_execution_start" ? event.args : undefined,
				event.type === "tool_execution_update" && record(event.partialResult)
					? contentText(event.partialResult.content)
					: undefined,
				false,
				true,
			);
		} else if (
			event.type === "tool_execution_end" &&
			typeof event.toolCallId === "string" &&
			typeof event.toolName === "string"
		) {
			this.tool(
				event.toolCallId,
				event.toolName,
				undefined,
				record(event.result) ? contentText(event.result.content) : "",
				event.isError,
				false,
			);
		}
		return before !== this.revision;
	}

	touch(): void {
		this.revision++;
	}

	markTruncated(): void {
		this.truncated = true;
	}

	snapshot(agent: ActiveAgentSummary, expiresAt?: number): ConversationSnapshot {
		const fields = [agent.agentName, agent.brief, agent.model, agent.result, agent.error];
		const truncated =
			this.truncated || fields.some((field) => field !== undefined && boundedText(field).truncated);
		return {
			agent: {
				...agent,
				agentName: boundedText(agent.agentName).text,
				brief: boundedText(agent.brief).text,
				model: agent.model === undefined ? undefined : boundedText(agent.model).text,
				result: agent.result === undefined ? undefined : boundedText(agent.result).text,
				error: agent.error === undefined ? undefined : boundedText(agent.error).text,
			},
			revision: this.revision,
			items: this.items.map((item) => ({ ...item })),
			truncated,
			...(expiresAt === undefined ? {} : { expiresAt }),
		};
	}

	private id(): string {
		return `message-${++this.nextId}`;
	}
	private field(value: string): string {
		const result = boundedText(value);
		this.truncated ||= result.truncated;
		return result.text;
	}
	private tool(
		id: string,
		name: string,
		args?: unknown,
		output?: string,
		isError = false,
		partial = true,
	): void {
		const safeId = this.field(id);
		const existing = this.items.find((item) => item.kind === "tool" && item.id === safeId);
		const previous = existing?.kind === "tool" ? existing : undefined;
		if (previous && !previous.partial && partial && output !== undefined) return;
		this.put({
			kind: "tool",
			id: safeId,
			name: this.field(name),
			arguments: args === undefined ? (previous?.arguments ?? "") : this.field(argumentsText(args)),
			output: output === undefined ? (previous?.output ?? "") : this.field(output),
			isError: output === undefined ? (previous?.isError ?? false) : isError,
			partial: output === undefined ? (previous?.partial ?? partial) : partial,
		});
	}
	private measureItem(
		item: ConversationItem,
		key: string,
	): { size: number; fields: Map<string, { value: string; size: number }> } {
		const previous = this.itemSizes.get(key);
		const fields = new Map<string, { value: string; size: number }>();
		let fieldBytes = 0;
		const empty = Object.fromEntries(
			Object.entries(item).map(([name, value]) => {
				if (typeof value !== "string") return [name, value];
				const cached = previous?.fields.get(name);
				const size = cached?.value === value ? cached.size : bytes(JSON.stringify(value)) - 2;
				fields.set(name, { value, size });
				fieldBytes += size;
				return [name, ""];
			}),
		);
		return { size: bytes(JSON.stringify(empty)) + fieldBytes, fields };
	}
	private put(item: ConversationItem): void {
		const key = `${item.kind}:${item.id}`;
		const index = this.items.findIndex((entry) => entry.id === item.id && entry.kind === item.kind);
		const previous = this.items[index];
		if (
			previous &&
			previous.kind === item.kind &&
			((previous.kind !== "tool" && item.kind !== "tool" && previous.text === item.text) ||
				(previous.kind === "tool" &&
					item.kind === "tool" &&
					previous.name === item.name &&
					previous.arguments === item.arguments &&
					previous.output === item.output &&
					previous.isError === item.isError &&
					previous.partial === item.partial))
		)
			return;
		let measurement = this.measureItem(item, key);
		if (measurement.size + 2 > CONVERSATION_MAX_BYTES && item.kind === "tool") {
			item = { ...item, name: "[truncated]", arguments: "[truncated]", output: "[truncated]" };
			measurement = this.measureItem(item, key);
			this.truncated = true;
		}
		if (index >= 0) {
			this.itemBytes -= this.itemSizes.get(key)?.size ?? 0;
			this.items[index] = item;
		} else {
			if (this.items.length) this.itemBytes++;
			this.items.push(item);
		}
		this.itemSizes.set(key, measurement);
		this.itemBytes += measurement.size;
		while (this.items.length > CONVERSATION_MAX_ITEMS || this.itemBytes > CONVERSATION_MAX_BYTES) {
			const removed = this.items.shift();
			if (!removed) break;
			const removedKey = `${removed.kind}:${removed.id}`;
			this.itemBytes -= (this.itemSizes.get(removedKey)?.size ?? 0) + (this.items.length ? 1 : 0);
			this.itemSizes.delete(removedKey);
			this.truncated = true;
		}
		this.revision++;
	}
}
