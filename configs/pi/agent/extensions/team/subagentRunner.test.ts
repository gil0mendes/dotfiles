import { AuthStorage, ModelRegistry } from "@earendil-works/pi-coding-agent";
import { describe, expect, it } from "vitest";
import { resolveModel, resolveTools } from "./subagentRunner";
import type { AgentConfig } from "./loader/types";

const modelRegistry = ModelRegistry.inMemory(AuthStorage.inMemory());

const createAgentConfig = (tools?: string[]): AgentConfig => ({
	name: "worker",
	description: "Completes a bounded task",
	type: "agent",
	systemPrompt: "Complete the task.",
	filePath: "/tmp/worker.md",
	...(tools === undefined ? {} : { tools }),
});

describe("resolveModel", () => {
	it("falls back once after every declared preference is unavailable", () => {
		const result = resolveModel(
			{
				...createAgentConfig(),
				parsedModels: [
					{ provider: "provider", modelId: "missing-one" },
					{ provider: "provider", modelId: "missing-two" },
				],
			},
			{
				model: undefined,
				modelRegistry,
				agentDir: "/agents",
				availableToolNames: [],
			},
		);

		expect(result).toEqual({
			model: undefined,
			warnings: [
				'Model preferences "provider/missing-one", "provider/missing-two" not available, using current session model',
			],
		});
	});
});

describe("resolveTools", () => {
	it("preserves undefined when the agent config omits tools", () => {
		const result = resolveTools(createAgentConfig(), ["read"]);

		expect(result).toEqual({ tools: undefined, warnings: [] });
	});

	it("preserves an explicit empty tools list", () => {
		const result = resolveTools(createAgentConfig([]), ["read"]);

		expect(result).toEqual({ tools: [], warnings: [] });
	});

	it("allows extension tools supplied by the runtime", () => {
		const result = resolveTools(createAgentConfig(["read", "web_search"]), ["read", "web_search"]);

		expect(result).toEqual({
			tools: ["read", "web_search"],
			warnings: [],
		});
	});

	it("filters unknown tools and reports each skipped declaration", () => {
		const result = resolveTools(createAgentConfig(["read", "not_registered", "another_unknown"]), [
			"read",
		]);

		expect(result).toEqual({
			tools: ["read"],
			warnings: [
				'Unknown tool "not_registered" in subagent config, skipping',
				'Unknown tool "another_unknown" in subagent config, skipping',
			],
		});
	});
});
