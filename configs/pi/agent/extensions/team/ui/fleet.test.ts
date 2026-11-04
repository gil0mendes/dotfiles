import { KeybindingsManager, TUI_KEYBINDINGS, visibleWidth } from "@earendil-works/pi-tui";
import { describe, expect, it } from "vitest";
import type { ActiveAgentSummary } from "../runtime";
import { createFleetOverlay } from "./overlay";
import { buildFleetLines, sanitizeFleetText } from "./presentation";
import { createFleetState, moveFleetSelection, refreshFleetState } from "./state";
import type { FleetView } from "./types";

const active = (
	id: string,
	status: ActiveAgentSummary["status"] = "running",
): ActiveAgentSummary => ({
	id,
	agentName: `agent-${id}`,
	brief: "A deliberately long brief that is safe to truncate at narrow widths",
	status,
	turns: 2,
	contextTokens: 40,
	model: "provider/model",
	startedAt: 0,
	result: undefined,
	error: undefined,
});

const view = (): FleetView => ({
	rows: [
		{ kind: "active", agent: active("one") },
		{ kind: "active", agent: active("two", "waiting") },
	],
	detachedActiveCount: 1,
});

describe("fleet state and presentation", () => {
	it("keeps stable id selection and renders safely at narrow widths", () => {
		const initial = refreshFleetState(createFleetState(), view(), 1);
		const moved = moveFleetSelection(initial, view(), 1, 1);
		expect(moved.selectedId).toBe("two");
		const lines = buildFleetLines(view(), moved, 5_000, 24, 1);
		expect(lines.every((line) => visibleWidth(line) <= 24)).toBe(true);
		expect(lines.join("\n")).toContain("Other active session");
		expect(lines.join("\n")).toContain("> agent-two");
	});

	it("uses the settled time for recent row durations", () => {
		const recentView: FleetView = {
			rows: [{ kind: "recent", agent: { ...active("done", "done"), settledAt: 5_000 } }],
			detachedActiveCount: 0,
		};
		const lines = buildFleetLines(recentView, createFleetState(), 60_000, 200, 8);
		expect(lines.join("\n")).toContain("5s");
		expect(lines.join("\n")).not.toContain("1m00s");
	});

	it("sanitizes row content before rendering and resets empty state", () => {
		expect(sanitizeFleetText("name\n\x1b[31mred\x1b[0m\ttext")).toBe("name red text");
		const sanitizedView: FleetView = {
			rows: [
				{
					kind: "recent",
					agent: {
						...active("safe", "error"),
						agentName: "name\n",
						brief: "brief\x1b[31m",
						model: "model\tname",
						error: "error\rmessage",
						result: "result\x07message",
						settledAt: 1_000,
					},
				},
			],
			detachedActiveCount: 0,
		};
		const lines = buildFleetLines(sanitizedView, createFleetState(), 2_000, 500, 8);
		expect(lines.join("\n")).toContain("name · recent error · model name · brief · error message");
		expect(lines.every((line) => !/[\u0000-\u001F\u007F-\u009F]/.test(line))).toBe(true);
		expect(
			refreshFleetState(
				{ selectedId: "stale", windowStart: 4 },
				{ rows: [], detachedActiveCount: 0 },
				8,
			),
		).toEqual(createFleetState());
	});

	it("handles configured keys with a real keybindings manager", () => {
		const keybindings = new KeybindingsManager(TUI_KEYBINDINGS, {
			"tui.select.up": "h",
			"tui.select.down": "l",
			"tui.select.cancel": "x",
		});
		const actions: string[] = [];
		const overlay = createFleetOverlay(
			{ requestRender: () => {} },
			keybindings,
			view,
			(action) => actions.push(action.kind === "respond" ? action.agentId : action.kind),
		);

		overlay.handleInput("l");
		overlay.handleInput("h");
		overlay.handleInput("j");
		overlay.handleInput("r");
		overlay.handleInput("x");
		expect(actions).toEqual(["two", "close"]);
	});

	it("turns selected waiting rows into a response action", () => {
		const renders: number[] = [];
		const actions: string[] = [];
		const overlay = createFleetOverlay(
			{ requestRender: () => renders.push(1) },
			{ matches: (data, action) => data === "down" && action === "tui.select.down" },
			view,
			(action) => actions.push(action.kind === "respond" ? action.agentId : action.kind),
		);
		overlay.handleInput("down");
		overlay.handleInput("r");
		expect(actions).toEqual(["two"]);
		expect(renders).toHaveLength(1);
	});
});
