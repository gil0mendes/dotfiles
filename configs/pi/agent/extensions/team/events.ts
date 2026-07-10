import type { EventBus } from "@earendil-works/pi-coding-agent";

export const TEAM_RUNTIME_CHANGED_EVENT = "team:runtime:changed";
export const TEAM_WARNING_EVENT = "team:warning";

export type TeamRuntimeChangeReason =
	| "activated"
	| "spawned"
	| "progressed"
	| "responded"
	| "settled"
	| "removed";

export type TeamRuntimeChangedPayload = {
	ownerSessionId: string;
	reason: TeamRuntimeChangeReason;
	agentId?: string;
};

export type TeamWarningPayload = {
	ownerSessionId: string;
	message: string;
};

export const emitTeamRuntimeChanged = (
	events: EventBus,
	payload: TeamRuntimeChangedPayload,
): void => {
	events.emit(TEAM_RUNTIME_CHANGED_EVENT, payload);
};

export const emitTeamWarning = (events: EventBus, payload: TeamWarningPayload): void => {
	events.emit(TEAM_WARNING_EVENT, payload);
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
	typeof value === "object" && value !== null;

const isRuntimeChangeReason = (value: unknown): value is TeamRuntimeChangeReason =>
	value === "activated" ||
	value === "spawned" ||
	value === "progressed" ||
	value === "responded" ||
	value === "settled" ||
	value === "removed";

export const isTeamRuntimeChangedPayload = (value: unknown): value is TeamRuntimeChangedPayload =>
	isRecord(value) &&
	typeof value.ownerSessionId === "string" &&
	isRuntimeChangeReason(value.reason) &&
	(value.agentId === undefined || typeof value.agentId === "string");

export const isTeamWarningPayload = (value: unknown): value is TeamWarningPayload =>
	isRecord(value) && typeof value.ownerSessionId === "string" && typeof value.message === "string";
