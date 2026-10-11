import { createEventBus } from "@earendil-works/pi-coding-agent";
import { describe, expect, it } from "vitest";
import { isTeamWarningPayload, TEAM_WARNING_EVENT } from "./events";
import { Runtime } from "./runtime";

describe("Runtime session bindings", () => {
	it("keeps a parent session bound while a child session starts and stops", () => {
		const runtime = new Runtime();
		const rootEvents = createEventBus();
		const childEvents = createEventBus();
		const warnings: string[] = [];
		rootEvents.on(TEAM_WARNING_EVENT, (payload) => {
			if (isTeamWarningPayload(payload)) {
				warnings.push(payload.message);
			}
		});

		runtime.activateSession({
			cwd: "/root",
			sessionId: "root",
			sendMessage: () => {},
			isIdle: () => true,
			events: rootEvents,
		});
		runtime.activateSession({
			cwd: "/child",
			sessionId: "child",
			sendMessage: () => {},
			isIdle: () => true,
			events: childEvents,
		});

		runtime.reportWarning("root", "while child is active");
		runtime.terminateSession("child");
		runtime.reportWarning("root", "after child stops");

		expect(warnings).toEqual(["while child is active", "after child stops"]);
	});
});
