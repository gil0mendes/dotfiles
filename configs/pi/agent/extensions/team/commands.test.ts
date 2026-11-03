import { describe, expect, it } from "vitest";
import { parseTeamCommandAction } from "./commands";

describe("/team command parsing", () => {
	it("uses the interactive menu when no action is supplied", () => {
		expect(parseTeamCommandAction("  ")).toBe("menu");
	});

	it("accepts supported actions regardless of whitespace or case", () => {
		expect(parseTeamCommandAction(" list ")).toBe("list");
		expect(parseTeamCommandAction("RESPOND")).toBe("respond");
		expect(parseTeamCommandAction("Abort")).toBe("abort");
	});

	it("rejects actions that do not have a confirmed safe flow", () => {
		expect(parseTeamCommandAction("abort all")).toBeUndefined();
	});
});
