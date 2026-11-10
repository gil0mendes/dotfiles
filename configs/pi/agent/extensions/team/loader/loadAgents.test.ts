import { describe, expect, it } from "vitest";
import { loadTeamConfig } from "../config";
import { loadAgentDefinitionFromFile } from "./loadAgents";
import { parseModel } from "./utils";

const loadAgentFromContent = (content: string) =>
	loadAgentDefinitionFromFile({
		filePath: "/tmp/test-agent.md",
		content,
	});

describe("model declaration parsing", () => {
	it("accepts bare selectors and rejects qualified queries without alphanumeric tokens", () => {
		expect(parseModel("gpt-5")).toEqual({ kind: "bare", raw: "gpt-5", query: "gpt-5" });
		expect(parseModel("provider/---")).toBeNull();
		expect(parseModel("provider/path/to-model")).toEqual({
			kind: "qualified",
			raw: "provider/path/to-model",
			provider: "provider",
			modelId: "path/to-model",
		});
	});
});

describe("mixed declarations", () => {
	it("preserves raw spelling and rejects malformed entries consistently in both sources", () => {
		const declarations =
			'["  claude sonnet 4_5  ", " P / path/to:model ", "---", "/x", "p/", "p/---", 42]';
		const agent = loadAgentFromContent(
			`---\nname: scout\ndescription: test\nmodel: ${declarations}\n---\nPrompt\n`,
		);
		const team = loadTeamConfig(`agents:\n  scout:\n    model: ${declarations}\n`);
		const expected = [
			{ kind: "bare", raw: "claude sonnet 4_5", query: "claude sonnet 4_5" },
			{ kind: "qualified", raw: "P / path/to:model", provider: "P", modelId: "path/to:model" },
			{ kind: "bare", raw: "---", query: "---" },
		];
		expect(agent.agent?.parsedModels).toEqual(expected);
		expect(team.agents.scout?.parsedModels).toEqual(expected);
		expect(team.warnings).toHaveLength(4);
		for (const value of ["", " ", "/", "/id", "p/", "p/---"]) expect(parseModel(value)).toBeNull();
		expect(parseModel("x*[a]:2024")).toEqual({
			kind: "bare",
			raw: "x*[a]:2024",
			query: "x*[a]:2024",
		});
	});
});

describe("loadAgentDefinitionFromFile", () => {
	it("keeps single string model declarations compatible", () => {
		const result = loadAgentFromContent(`---
name: scout
description: Investigates codebase
model: openai-codex/gpt-5.5
---
Prompt
`);

		expect(result.agent?.type).toBe("agent");
		expect(result.agent?.model).toBe("openai-codex/gpt-5.5");
		expect(result.agent?.parsedModel).toEqual({
			kind: "qualified",
			raw: "openai-codex/gpt-5.5",
			provider: "openai-codex",
			modelId: "gpt-5.5",
		});
		expect(result.agent?.parsedModels).toEqual([parseModel("openai-codex/gpt-5.5")]);
		expect(result.warnings).toEqual([]);
	});

	it("warns consistently for invalid frontmatter model declarations", () => {
		const result = loadAgentFromContent(`---
name: scout
description: Investigates codebase
model: provider/---
---
Prompt
`);

		expect(result.agent?.parsedModels).toBeUndefined();
		expect(result.warnings[0]?.message).toContain("alphanumeric query token");
	});

	it("accepts orchestrator type declarations", () => {
		const result = loadAgentFromContent(`---
name: coordinator
description: Coordinates subagents
type: orchestrator
---
Prompt
`);

		expect(result.agent?.type).toBe("orchestrator");
		expect(result.warnings).toEqual([]);
	});

	it("rejects unsupported type declarations", () => {
		const result = loadAgentFromContent(`---
name: worker
description: Completes tasks
type: supervisor
---
Prompt
`);

		expect(result.agent).toBeNull();
		expect(result.warnings).toEqual([
			{
				filePath: "/tmp/test-agent.md",
				message:
					'Ignored subagent definition "worker". Frontmatter field "type" must be either "agent" or "orchestrator".',
			},
		]);
	});

	it("parses declared delegation permissions", () => {
		const result = loadAgentFromContent(`---
name: coordinator
description: Coordinates research
delegatesTo: [explorer, researcher]
rootOnly: true
---
Prompt
`);

		expect(result.agent?.delegatesTo).toEqual(["explorer", "researcher"]);
		expect(result.agent?.rootOnly).toBe(true);
		expect(result.warnings).toEqual([]);
	});

	it("preserves declared tools for runtime validation", () => {
		const result = loadAgentFromContent(`---
name: extension-user
description: Uses an extension tool
tools: [read, web_search]
---
Prompt
`);

		expect(result.agent?.tools).toEqual(["read", "web_search"]);
		expect(result.warnings).toEqual([]);
	});

	it("parses ordered model preference arrays", () => {
		const result = loadAgentFromContent(`---
name: oracle
description: Challenges assumptions
model:
  - anthropic/claude-opus-4-5
  - openai-codex/gpt-5.5
---
Prompt
`);

		expect(result.agent?.model).toEqual(["anthropic/claude-opus-4-5", "openai-codex/gpt-5.5"]);
		expect(result.agent?.parsedModel).toEqual({
			kind: "qualified",
			raw: "anthropic/claude-opus-4-5",
			provider: "anthropic",
			modelId: "claude-opus-4-5",
		});
		expect(result.agent?.parsedModels).toEqual([
			parseModel("anthropic/claude-opus-4-5"),
			parseModel("openai-codex/gpt-5.5"),
		]);
		expect(result.warnings).toEqual([]);
	});
});

describe("loadTeamConfig", () => {
	it("warns for invalid YAML model declarations instead of silently dropping them", () => {
		const config = loadTeamConfig(`agents:
  oracle:
    model: provider/---
`);

		expect(config.agents.oracle?.parsedModels).toBeUndefined();
		expect(config.warnings).toEqual([
			{
				agentName: "oracle",
				issue: { value: "provider/---", reason: "invalid-selector" },
			},
		]);
	});

	it("parses ordered model preference arrays in overrides", () => {
		const config = loadTeamConfig(`agents:
  oracle:
    model:
      - anthropic/claude-opus-4-5
      - openai-codex/gpt-5.5
`);

		expect(config.agents.oracle?.model).toEqual([
			"anthropic/claude-opus-4-5",
			"openai-codex/gpt-5.5",
		]);
		expect(config.agents.oracle?.parsedModels).toEqual([
			parseModel("anthropic/claude-opus-4-5"),
			parseModel("openai-codex/gpt-5.5"),
		]);
	});
});
