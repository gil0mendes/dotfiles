import { createEventBus } from "@earendil-works/pi-coding-agent";
import { describe, expect, it } from "vitest";
import { TEAM_RUNTIME_CHANGED_EVENT, TEAM_WARNING_EVENT } from "../events";
import type { ActiveAgentSummary } from "../runtime";
import {
	buildAgentStatusPresentation,
	createTeamEventSubscriptionLifecycle,
	formatElapsed,
} from "./index";

const agent = (id: string, status: ActiveAgentSummary["status"]): ActiveAgentSummary => ({
	id,
	agentName: `agent-${id}`,
	status,
	turns: 2,
	contextTokens: 120,
	model: undefined,
	startedAt: 10_000,
});

describe("team status presentation", () => {
	it("removes event listeners at shutdown and registers them for the next session", () => {
		const events = createEventBus();
		const receivedEvents: string[] = [];
		const subscriptions = createTeamEventSubscriptionLifecycle(events, {
			onRuntimeChanged: () => receivedEvents.push("runtime"),
			onWarning: () => receivedEvents.push("warning"),
		});

		subscriptions.subscribe();
		events.emit(TEAM_RUNTIME_CHANGED_EVENT, { ownerSessionId: "one", reason: "activated" });
		subscriptions.dispose();
		events.emit(TEAM_WARNING_EVENT, { ownerSessionId: "one", message: "stale" });
		subscriptions.subscribe();
		events.emit(TEAM_WARNING_EVENT, { ownerSessionId: "two", message: "fresh" });

		expect(receivedEvents).toEqual(["runtime", "warning"]);
	});

	it("formats elapsed time without negative values", () => {
		expect(formatElapsed(10_000, 5_000)).toBe("0s");
		expect(formatElapsed(10_000, 75_000)).toBe("1m05s");
	});

	it("keeps the widget bounded and reports hidden active agents", () => {
		const presentation = buildAgentStatusPresentation(
			[
				agent("one", "running"),
				agent("two", "waiting"),
				agent("three", "running"),
				agent("four", "running"),
				agent("five", "running"),
			],
			20_000,
			"⠋",
		);

		expect(presentation.rows).toHaveLength(5);
		expect(presentation.rows.at(-1)).toBe("… 1 more active agent(s)");
		expect(presentation.footer).toBe("⠋ 5 agent(s) · 1 waiting");
		expect(presentation.hasRunningAgents).toBe(true);
	});

	it("truncates long agent names before rendering", () => {
		const longNamedAgent = {
			...agent("long", "waiting"),
			agentName: "this-agent-name-is-intentionally-much-too-long-to-display",
		};
		const presentation = buildAgentStatusPresentation([longNamedAgent], 20_000, "⠋");

		expect(presentation.rows[0]).toContain("this-agent-name-is-inte…");
		expect(presentation.hasRunningAgents).toBe(false);
	});
});
