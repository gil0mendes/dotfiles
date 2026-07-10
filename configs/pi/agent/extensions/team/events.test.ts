import { createEventBus } from "@earendil-works/pi-coding-agent";
import { describe, expect, it } from "vitest";
import {
	emitTeamRuntimeChanged,
	emitTeamWarning,
	isTeamRuntimeChangedPayload,
	isTeamWarningPayload,
	TEAM_RUNTIME_CHANGED_EVENT,
	TEAM_WARNING_EVENT,
} from "./events";

describe("team events", () => {
	it("emits runtime changes with their owner session", () => {
		const events = createEventBus();
		let received: unknown;
		events.on(TEAM_RUNTIME_CHANGED_EVENT, (event) => {
			received = event;
		});

		emitTeamRuntimeChanged(events, {
			ownerSessionId: "session-1",
			reason: "spawned",
			agentId: "scout-1",
		});

		expect(isTeamRuntimeChangedPayload(received)).toBe(true);
	});

	it("emits and validates warnings", () => {
		const events = createEventBus();
		let received: unknown;
		events.on(TEAM_WARNING_EVENT, (event) => {
			received = event;
		});

		emitTeamWarning(events, {
			ownerSessionId: "session-1",
			message: "Unknown skill",
		});

		expect(isTeamWarningPayload(received)).toBe(true);
		expect(isTeamRuntimeChangedPayload({ ownerSessionId: "session-1", reason: "unknown" })).toBe(
			false,
		);
	});
});
