import { describe, expect, it, vi } from "vitest";
import { handleTeamCommand, parseTeamCommandAction } from "./commands";
import { Runtime } from "./runtime";

type TeamCommandContext = Parameters<typeof handleTeamCommand>[1];

const createContext = (hasUI: boolean, notifications: string[], selections: string[]) => {
	const context: TeamCommandContext = {
		hasUI,
		sessionManager: { getSessionId: () => "owner" },
		ui: {
			notify: (message) => notifications.push(message),
			custom: async () => {
				throw new Error("Custom overlays are unavailable");
			},
			select: async (title) => {
				selections.push(title);
				return undefined;
			},
			input: async () => undefined,
			confirm: async () => false,
		},
	};
	return context;
};

describe("/team command parsing", () => {
	it("uses the interactive menu when no action is supplied", () => {
		expect(parseTeamCommandAction("  ")).toBe("menu");
	});

	it("accepts supported actions regardless of whitespace or case", () => {
		expect(parseTeamCommandAction(" list ")).toBe("list");
		expect(parseTeamCommandAction("RESPOND")).toBe("respond");
		expect(parseTeamCommandAction("Abort")).toBe("abort");
		expect(parseTeamCommandAction("fleet")).toBe("fleet");
	});

	it("rejects actions that do not have a confirmed safe flow", () => {
		expect(parseTeamCommandAction("abort all")).toBeUndefined();
	});

	it("prints non-UI list and usage output durably", async () => {
		const output = vi.spyOn(console, "log").mockImplementation(() => {});
		const notifications: string[] = [];
		const selections: string[] = [];
		const context = createContext(false, notifications, selections);
		const runtime = new Runtime();

		await handleTeamCommand("list", context, runtime, async () => {});
		await handleTeamCommand("unknown", context, runtime, async () => {});
		await handleTeamCommand("fleet", context, runtime, async () => {});

		expect(output.mock.calls.map(([message]) => message)).toEqual([
			"No active agents in this session.",
			"Usage: /team [list|fleet|respond|abort]",
			"/team fleet requires interactive UI mode.",
		]);
		expect(notifications).toEqual([]);
		expect(selections).toEqual([]);
		output.mockRestore();
	});

	it("notifies the UI roster without a modal before opening the bare action menu", async () => {
		const notifications: string[] = [];
		const selections: string[] = [];
		const context = createContext(true, notifications, selections);

		await handleTeamCommand("list", context, new Runtime(), async () => {});
		await handleTeamCommand("", context, new Runtime(), async () => {});

		expect(notifications).toEqual([
			"No active agents in this session.",
			"No active agents in this session.",
		]);
		expect(selections).toEqual(["Team actions"]);
	});
});
