import { truncateToWidth } from "@earendil-works/pi-tui";
import { sanitizeTerminalText } from "./sanitize";
import { formatElapsed } from "./formatElapsed";
import type { FleetState } from "./state";
import type { FleetView } from "./types";

export const sanitizeFleetText = (value: string): string =>
	sanitizeTerminalText(value.replace(/[\t\r]/g, " "))
		.replace(/\s+/g, " ")
		.trim();

const formatRow = (
	selected: boolean,
	kind: "active" | "recent",
	view: FleetView,
	index: number,
	now: number,
): string => {
	const row = view.rows[index];
	if (!row) return "";
	const agent = row.agent;
	const state = kind === "active" ? agent.status : `recent ${agent.status}`;
	const model = sanitizeFleetText(agent.model ?? "") || "model pending";
	const brief = sanitizeFleetText(agent.brief) || "No brief";
	const agentName = sanitizeFleetText(agent.agentName) || "unnamed agent";
	const outcome = sanitizeFleetText(agent.error ?? agent.result ?? "");
	const elapsedEnd = row.kind === "recent" ? row.agent.settledAt : now;
	const outcomeLabel = outcome ? ` · ${outcome}` : "";
	return `${selected ? ">" : " "} ${agentName} · ${state} · ${model} · ${brief}${outcomeLabel} · ${formatElapsed(agent.startedAt, elapsedEnd)} · ${agent.turns}t`;
};

export const buildFleetLines = (
	view: FleetView,
	state: FleetState,
	now: number,
	width: number,
	windowSize: number,
): string[] => {
	const lines = ["Team fleet"];
	if (view.detachedActiveCount > 0) {
		lines.push(
			`Other active session owners: ${view.detachedActiveCount} active agent(s) (read-only)`,
		);
	}
	if (view.rows.length === 0) {
		lines.push("No current or recent agents in this session.");
	} else {
		const rows = view.rows.slice(state.windowStart, state.windowStart + windowSize);
		for (const [offset, row] of rows.entries()) {
			const index = state.windowStart + offset;
			lines.push(formatRow(row.agent.id === state.selectedId, row.kind, view, index, now));
		}
		if (state.windowStart > 0) lines.push(`… ${state.windowStart} earlier row(s)`);
		const hiddenAfter = view.rows.length - state.windowStart - rows.length;
		if (hiddenAfter > 0) lines.push(`… ${hiddenAfter} more row(s)`);
	}
	lines.push("↑/↓ select · Enter/v view · r respond · a abort · Esc close");
	return lines.map((line) => truncateToWidth(line, width));
};
