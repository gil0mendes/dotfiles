import { fleetKeyAction } from "./keys";
import { buildFleetLines } from "./presentation";
import {
	createFleetState,
	moveFleetSelection,
	refreshFleetState,
	selectedFleetRow,
	type FleetState,
} from "./state";
import type { FleetAction, FleetView } from "./types";

type OverlayTui = { requestRender: () => void };
type OverlayKeys = {
	matches: (
		data: string,
		action: "tui.select.up" | "tui.select.down" | "tui.select.cancel" | "tui.select.confirm",
	) => boolean;
};

export type FleetOverlay = {
	render: (width: number) => string[];
	handleInput: (data: string) => void;
	invalidate: () => void;
	refresh: () => void;
};

const WINDOW_SIZE = 8;

export const createFleetOverlay = (
	tui: OverlayTui,
	keybindings: OverlayKeys,
	getView: () => FleetView,
	done: (action: FleetAction) => void,
	now: () => number = Date.now,
	initialState: FleetState = createFleetState(),
	onState: (state: FleetState) => void = () => {},
	isAuthorized: () => boolean = () => true,
): FleetOverlay => {
	let view = getView();
	let state = refreshFleetState(initialState, view, WINDOW_SIZE);
	let cachedWidth: number | undefined;
	let cachedLines: string[] | undefined;

	const invalidate = (): void => {
		cachedWidth = undefined;
		cachedLines = undefined;
	};
	const refresh = (): void => {
		view = getView();
		state = refreshFleetState(state, view, WINDOW_SIZE);
		onState(state);
		invalidate();
		tui.requestRender();
	};

	return {
		render: (width) => {
			if (!isAuthorized()) return [];
			if (cachedWidth === width && cachedLines) return cachedLines;
			cachedWidth = width;
			cachedLines = buildFleetLines(view, state, now(), width, WINDOW_SIZE);
			return cachedLines;
		},
		handleInput: (data) => {
			if (!isAuthorized()) {
				done({ kind: "close" });
				return;
			}
			const action = fleetKeyAction(data, (input, binding) => keybindings.matches(input, binding));
			if (action === "none") return;
			if (action === "close") {
				done({ kind: "close" });
				return;
			}
			if (action === "up" || action === "down") {
				state = moveFleetSelection(state, view, action === "up" ? -1 : 1, WINDOW_SIZE);
				onState(state);
				invalidate();
				tui.requestRender();
				return;
			}
			const selected = selectedFleetRow(state, view);
			if (!selected) return;
			if (action === "view") {
				onState(state);
				done({ kind: "view", agentId: selected.agent.id });
				return;
			}
			if (selected.kind !== "active") return;
			if (action === "respond" && selected.agent.status === "waiting") {
				done({ kind: "respond", agentId: selected.agent.id });
				return;
			}
			if (
				action === "abort" &&
				(selected.agent.status === "running" || selected.agent.status === "waiting")
			) {
				done({ kind: "abort", agentId: selected.agent.id });
			}
		},
		invalidate,
		refresh,
	};
};
