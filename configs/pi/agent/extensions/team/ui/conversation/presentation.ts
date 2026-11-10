import { getMarkdownTheme } from "@earendil-works/pi-coding-agent";
import { Markdown, truncateToWidth, wrapTextWithAnsi } from "@earendil-works/pi-tui";
import {
	CONVERSATION_MAX_FIELD_BYTES,
	CONVERSATION_MAX_ITEMS,
	type ConversationItem,
	type ConversationSnapshot,
} from "../../conversation";
import { sanitizeTerminalText } from "../sanitize";

export const sanitizeConversationText = (value: string): string => sanitizeTerminalText(value);

function wrapped(text: string, width: number): string[] {
	return sanitizeConversationText(text)
		.split("\n")
		.flatMap((line) => (line ? wrapTextWithAnsi(line, width) : [""]));
}

function textLines(text: string, width: number, markdown: boolean): string[] {
	const safe = sanitizeConversationText(text);
	if (markdown) {
		try {
			return new Markdown(safe, 0, 0, getMarkdownTheme())
				.render(width)
				.map((line) => truncateToWidth(sanitizeTerminalText(line, true), width));
		} catch {
			return ["[Markdown unavailable; showing raw text]", ...wrapped(safe, width)];
		}
	}
	return wrapped(safe, width);
}

function renderItem(
	item: ConversationItem,
	width: number,
	expanded: boolean,
	markdown: boolean,
): string[] {
	if (item.kind !== "tool") {
		return [
			item.kind === "user" ? "USER" : "ASSISTANT",
			...textLines(item.text || "[empty]", width, markdown),
			"",
		];
	}
	const lines = wrapped(
		`${item.isError ? "✗" : item.partial ? "◌" : "◆"} TOOL ${item.name} [${item.id}]${item.partial ? " (partial)" : ""}`,
		width,
	);
	if (expanded) {
		lines.push("Arguments:", ...wrapped(item.arguments || "[none]", width));
		lines.push(
			"Output:",
			...wrapped(item.output || (item.partial ? "[pending]" : "[empty]"), width),
		);
	} else lines.push("[tool details collapsed]");
	lines.push("");
	return lines.map((line) => truncateToWidth(sanitizeTerminalText(line, true), width));
}

function fields(item: ConversationItem, expanded: boolean): (string | boolean)[] {
	if (item.kind !== "tool") return [item.text];
	return expanded
		? [item.name, item.isError, item.partial, item.arguments, item.output]
		: [item.name, item.isError, item.partial];
}

type CacheEntry = { fields: (string | boolean)[]; lines: string[] };

export class ConversationRenderCache {
	private entries = new Map<string, CacheEntry>();
	private options = "";

	clear(): void {
		this.entries.clear();
		this.options = "";
	}

	lines(
		snapshot: ConversationSnapshot,
		width: number,
		expanded: boolean,
		markdown: boolean,
	): string[] {
		const options = `${width}:${expanded}:${markdown}`;
		if (this.options !== options) {
			this.clear();
			this.options = options;
		}
		const next = new Map<string, CacheEntry>();
		const lines: string[] = [];
		if (snapshot.truncated) lines.push("[Earlier content or fields truncated]", "");
		if (!snapshot.items.length) lines.push("No conversation content yet.");
		for (const item of snapshot.items) {
			const key = JSON.stringify([item.kind, item.id]);
			const values = fields(item, expanded);
			const old = this.entries.get(key);
			const rendered =
				old &&
				values.length === old.fields.length &&
				values.every((value, i) => value === old.fields[i])
					? old.lines
					: renderItem(item, width, expanded, markdown);
			lines.push(...rendered);
			if (
				next.size < CONVERSATION_MAX_ITEMS &&
				key.length <= CONVERSATION_MAX_FIELD_BYTES &&
				values.every(
					(value) => typeof value !== "string" || value.length <= CONVERSATION_MAX_FIELD_BYTES,
				)
			) {
				next.set(key, { fields: values, lines: rendered });
			}
		}
		this.entries = next;
		const outcome =
			snapshot.agent.error ||
			(!snapshot.items.some((item) => item.kind === "assistant" && item.text.trim())
				? snapshot.agent.result
				: undefined);
		if (outcome)
			lines.push(snapshot.agent.error ? "ERROR" : "RESULT", ...wrapped(outcome, width), "");
		return lines.map((line) => truncateToWidth(sanitizeTerminalText(line, true), width));
	}
}

export function buildConversationLines(
	snapshot: ConversationSnapshot | undefined,
	width: number,
	expanded: boolean,
	markdown: boolean,
	cache = new ConversationRenderCache(),
): string[] {
	const columns = Math.max(1, width);
	if (!snapshot) {
		cache.clear();
		return [truncateToWidth("Conversation unavailable or expired.", columns)];
	}
	return cache.lines(snapshot, columns, expanded, markdown);
}
