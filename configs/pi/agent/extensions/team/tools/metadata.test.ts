import { describe, expect, it } from "vitest";
import { teamListToolMetadata } from "./list";
import { teamSpawnToolMetadata } from "./spawn";

describe("team tool prompt guidance", () => {
	it("allows direct spawning from the injected routing snapshot", () => {
		expect(teamSpawnToolMetadata).toEqual({
			description:
				"Spawn a non-blocking subagent in an isolated session. team_spawn reloads configuration and enforces agent existence and delegation permission before asynchronous runner startup. During startup, model and tool resolution may fall back and emit warnings. Results return as steering messages.",
			subagentParameterDescription:
				"Known permitted subagent name; root may use its injected routing roster, or use a name discovered with team_list",
			briefParameterDescription:
				"Concise task label for session lists, ideally under 80 characters. This is not the full task.",
			taskParameterDescription: "Task to delegate to the subagent",
			promptSnippet:
				"Spawn a non-blocking subagent directly by a known permitted name. Root may use its injected routing roster; team_list is not required first.",
			promptGuidelines: [
				"team_spawn: Spawn directly by a known permitted name. For root, the injected routing roster is one source; do not call team_list first solely to authorize a spawn.",
				"team_spawn: Reloads configuration and enforces agent existence and delegation permission before asynchronous runner startup. During startup, model and tool resolution may fall back and emit warnings.",
				"team_spawn: Use team_list only when you need discovery, declared capability detail, or your caller-owned active status to avoid duplicate work; its snapshot does not guarantee availability or freshness.",
				"team_spawn: Keep brief short; put necessary context and criteria in task.",
				"team_spawn: Do not duplicate delegated work; wait for steering results.",
			],
		});
	});

	it("does not require team_list before spawning by a known permitted name", () => {
		const guidance = [
			teamSpawnToolMetadata.description,
			teamSpawnToolMetadata.subagentParameterDescription,
			teamSpawnToolMetadata.promptSnippet,
			...teamSpawnToolMetadata.promptGuidelines,
		].join("\n");

		expect(guidance).not.toMatch(/must call team_list before.*team_spawn/i);
		expect(guidance).not.toMatch(/team_list.*(?:must|required).*before.*team_spawn/i);
	});

	it("limits team_list to its snapshot use cases without claiming freshness", () => {
		expect(teamListToolMetadata).toEqual({
			description:
				"List declared subagent definitions and caller-owned active subagents as a snapshot.",
			promptSnippet:
				"Discover subagents, inspect their declared capabilities, or check your caller-owned active work; this snapshot does not guarantee availability or freshness.",
			promptGuidelines: [
				"team_list: Recommended only for discovery, declared capability detail, or a caller-owned active status and duplicate-work check.",
				"team_list: Its results are a snapshot and do not guarantee availability or freshness; team_spawn reloads configuration and enforces agent existence and delegation permission, while model and tool resolution occurs during asynchronous runner startup with fallback or warnings.",
			],
		});
	});
});
