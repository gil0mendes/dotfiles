import { truncateToWidth } from "@earendil-works/pi-tui";
import type { ConversationSnapshot } from "../../conversation";
import { conversationKeyAction, type ConversationKeys } from "./keys";
import {
	buildConversationLines,
	ConversationRenderCache,
	sanitizeConversationText,
} from "./presentation";
import { sanitizeTerminalText } from "../sanitize";
import { createConversationState, moveConversation, positionConversation } from "./state";

type ConversationTui = { requestRender: () => void; terminal?: { rows: number } };

export function createConversationOverlay(
	tui: ConversationTui,
	keys: ConversationKeys,
	getSnapshot: () => ConversationSnapshot | undefined,
	done: () => void,
	onSnapshot: (snapshot: ConversationSnapshot | undefined) => void = () => {},
) {
	let state = createConversationState();
	const cache = new ConversationRenderCache();
	const snapshotSafely = (): ConversationSnapshot | undefined => {
		try {
			return getSnapshot();
		} catch {
			return undefined;
		}
	};
	let count = 0;
	let height = 1;
	const invalidate = (): void => {
		cache.clear();
	};
	const refresh = (): void => {
		tui.requestRender();
	};
	return {
		refresh,
		invalidate,
		render(width: number): string[] {
			const columns = Math.max(1, width);
			const snapshot = snapshotSafely();
			onSnapshot(snapshot);
			const rows = tui.terminal?.rows;
			const maxHeight = Math.max(
				1,
				Math.floor((Number.isFinite(rows) && rows && rows > 0 ? rows : 24) * 0.8),
			);
			height = Math.max(1, maxHeight - 3);
			const lines = buildConversationLines(
				snapshot,
				columns,
				state.expanded,
				state.markdown,
				cache,
			);
			count = lines.length;
			state = positionConversation(state, count, height);
			const header = snapshot
				? `${sanitizeConversationText(snapshot.agent.agentName).replace(/\n/g, " ")} · ${snapshot.agent.status} · ${sanitizeConversationText(snapshot.agent.model ?? "model pending").replace(/\n/g, " ")}`
				: "Team conversation";
			const brief = snapshot
				? sanitizeConversationText(snapshot.agent.brief).replace(/\n/g, " ")
				: "";
			const footer = `↑↓/Pg scroll · End follow${state.follow ? " ●" : ""} · t tools · m ${state.markdown ? "raw" : "markdown"} · Esc back`;
			return [header, brief, ...lines.slice(state.offset, state.offset + height), footer]
				.slice(0, maxHeight)
				.map((line) =>
					truncateToWidth(sanitizeTerminalText(line, true).replace(/\n/g, ""), columns),
				);
		},
		handleInput(data: string): void {
			const action = conversationKeyAction(data, keys);
			if (action === "close") {
				done();
				return;
			}
			if (action === "none") return;
			if (!snapshotSafely()) {
				invalidate();
				refresh();
				return;
			}
			state = moveConversation(state, action, count, height);
			refresh();
		},
	};
}
