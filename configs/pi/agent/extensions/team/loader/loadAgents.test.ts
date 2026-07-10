import { describe, expect, it } from "vitest";
import { loadTeamConfig } from "../config";
import { loadAgentDefinitionFromFile } from "./loadAgents";

const loadAgentFromContent = (content: string) =>
	loadAgentDefinitionFromFile({
		filePath: "/tmp/test-agent.md",
		content,
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

		expect(result.agent?.model).toBe("openai-codex/gpt-5.5");
		expect(result.agent?.parsedModel).toEqual({
			provider: "openai-codex",
			modelId: "gpt-5.5",
		});
		expect(result.agent?.parsedModels).toEqual([
			{ provider: "openai-codex", modelId: "gpt-5.5" },
		]);
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

		expect(result.agent?.model).toEqual([
			"anthropic/claude-opus-4-5",
			"openai-codex/gpt-5.5",
		]);
		expect(result.agent?.parsedModel).toEqual({
			provider: "anthropic",
			modelId: "claude-opus-4-5",
		});
		expect(result.agent?.parsedModels).toEqual([
			{ provider: "anthropic", modelId: "claude-opus-4-5" },
			{ provider: "openai-codex", modelId: "gpt-5.5" },
		]);
		expect(result.warnings).toEqual([]);
	});
});

describe("loadTeamConfig", () => {
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
			{ provider: "anthropic", modelId: "claude-opus-4-5" },
			{ provider: "openai-codex", modelId: "gpt-5.5" },
		]);
	});
});
