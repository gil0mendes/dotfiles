import { createEventBus } from "@earendil-works/pi-coding-agent";
import { describe, expect, it } from "vitest";
import { Runtime } from "../runtime";
import { createFleetController } from "./controller";

type FleetCommandContext = Parameters<ReturnType<typeof createFleetController>["show"]>[0];

describe("fleet controller", () => {
	it("reports an unavailable custom overlay and releases its lifecycle resources", async () => {
		const notifications: string[] = [];
		const events = createEventBus();
		const controller = createFleetController(events, new Runtime());
		const context: FleetCommandContext = {
			sessionManager: { getSessionId: () => "owner" },
			ui: {
				notify: (message) => notifications.push(message),
				custom: async () => {
					throw new Error("Custom overlays are unavailable");
				},
				input: async () => undefined,
				confirm: async () => false,
			},
		};

		await controller.show(context);
		controller.dispose();

		expect(notifications).toEqual([
			"Fleet overlay is unavailable in this interface. Use /team list for the roster.",
		]);
	});
});
