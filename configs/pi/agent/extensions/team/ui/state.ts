import type { FleetRow, FleetView } from "./types";

export type FleetState = {
	selectedId: string | undefined;
	windowStart: number;
};

export const createFleetState = (): FleetState => ({ selectedId: undefined, windowStart: 0 });

export const selectedFleetRow = (state: FleetState, view: FleetView): FleetRow | undefined => {
	if (!state.selectedId) return undefined;
	return view.rows.find((row) => row.agent.id === state.selectedId);
};

export const refreshFleetState = (
	state: FleetState,
	view: FleetView,
	windowSize: number,
): FleetState => {
	if (view.rows.length === 0) return createFleetState();

	const selected = selectedFleetRow(state, view);
	const selectedId = selected ? selected.agent.id : view.rows[0]?.agent.id;
	const selectedIndex = view.rows.findIndex((row) => row.agent.id === selectedId);
	const windowStart = Math.max(
		0,
		Math.min(state.windowStart, Math.max(0, view.rows.length - windowSize)),
	);
	if (selectedIndex < windowStart) return { selectedId, windowStart: selectedIndex };
	if (selectedIndex >= windowStart + windowSize) {
		return { selectedId, windowStart: selectedIndex - windowSize + 1 };
	}
	return { selectedId, windowStart };
};

export const moveFleetSelection = (
	state: FleetState,
	view: FleetView,
	delta: number,
	windowSize: number,
): FleetState => {
	if (view.rows.length === 0) return refreshFleetState(state, view, windowSize);
	const currentIndex = Math.max(
		0,
		view.rows.findIndex((row) => row.agent.id === state.selectedId),
	);
	const selectedIndex = (currentIndex + delta + view.rows.length) % view.rows.length;
	return refreshFleetState(
		{ selectedId: view.rows[selectedIndex]?.agent.id, windowStart: state.windowStart },
		view,
		windowSize,
	);
};
