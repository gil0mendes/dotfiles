import { describe, expect, it } from "vitest";
import { canSpawnSubagent, getPermittedSubagents } from "./delegationPolicy";
import type { AgentConfig } from "./loader/types";

const createAgent = (
	name: string,
	delegatesTo?: string[],
	rootOnly = false,
): AgentConfig => ({
	name,
	description: `${name} agent`,
	type: "agent",
	systemPrompt: "",
	filePath: `/tmp/${name}.md`,
	delegatesTo,
	rootOnly,
});

describe("delegation policy", () => {
	const planner = createAgent("planner", [
		"explorer",
		"librarian",
		"researcher",
		"reviewer",
		"scribe",
	]);
	const coder = createAgent("coder");
	const explorer = createAgent("explorer");

	it("allows the root session to spawn any configured agent", () => {
		expect(getPermittedSubagents(undefined, [planner, coder])).toEqual([
			planner,
			coder,
		]);
	});

	it("allows a planner to delegate only to its configured research agents", () => {
		expect(getPermittedSubagents(planner, [planner, coder, explorer])).toEqual([
			explorer,
		]);
		expect(canSpawnSubagent(planner, coder)).toBe(false);
	});

	it("prevents workers from spawning agents by default", () => {
		expect(getPermittedSubagents(coder, [planner, explorer])).toEqual([]);
	});

	it("prevents non-root sessions from spawning root-only agents", () => {
		const rootOnlyCoder = createAgent("coder", undefined, true);
		const permittedPlanner = createAgent("planner", ["coder"]);
		expect(canSpawnSubagent(permittedPlanner, rootOnlyCoder)).toBe(false);
	});

	it("never permits an agent to spawn itself", () => {
		const recursivePlanner = createAgent("planner", ["planner"]);
		expect(canSpawnSubagent(recursivePlanner, recursivePlanner)).toBe(false);
	});
});
