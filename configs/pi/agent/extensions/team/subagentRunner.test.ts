import { getModel } from "@earendil-works/pi-ai/compat";
import { describe, expect, it } from "vitest";
import { resolveModel, resolveTools } from "./subagentRunner";
import type { AgentConfig } from "./loader/types";
import { parseModel } from "./loader/utils";

const selector = (value: string) => {
	const parsed = parseModel(value);
	if (!parsed) throw new Error(`Invalid test selector: ${value}`);
	return parsed;
};

const createAgentConfig = (tools?: string[]): AgentConfig => ({
	name: "worker",
	description: "Completes a bounded task",
	type: "agent",
	systemPrompt: "Complete the task.",
	filePath: "/tmp/worker.md",
	...(tools === undefined ? {} : { tools }),
});

describe("resolveModel", () => {
	it("treats explicit bare selection of the current identity as a preference, not inheritance", () => {
		const current = getModel("openai", "gpt-4o");
		if (!current) throw new Error("Missing test model");
		const child = { ...current };
		const result = resolveModel(
			{ ...createAgentConfig(), parsedModels: [selector("gpt-4o")] },
			{ model: current, availableModels: [child] },
		);
		expect(result.model).toBe(child);
		expect(result.usedCurrentModel).toBe(false);
	});
	it("falls back once after every declared preference is unavailable", () => {
		const model = getModel("openai", "gpt-4o");
		if (!model) throw new Error("Missing test model");
		const result = resolveModel(
			{
				...createAgentConfig(),
				parsedModels: [selector("provider/missing-one"), selector("provider/missing-two")],
			},
			{
				availableModels: [model],
			},
		);

		expect(result).toEqual({
			model: undefined,
			usedCurrentModel: false,
			warnings: [
				'Model selector "provider/missing-one" is unavailable, skipping',
				'Model selector "provider/missing-two" is unavailable, skipping',
				'Model preferences "provider/missing-one", "provider/missing-two" not available, using current session model',
			],
		});
	});
	it("rejects an inherited model absent from the child catalog", () => {
		const current = getModel("openai", "gpt-4o");
		if (!current) throw new Error("Missing test model");
		expect(() =>
			resolveModel(createAgentConfig(), { model: current, availableModels: [] }),
		).toThrow(/unavailable in the child runtime/);
	});

	it("rejects an empty catalog when no current model exists", () => {
		expect(() => resolveModel(createAgentConfig(), { availableModels: [] })).toThrow(
			/No authenticated models/,
		);
	});

	it("falls back to a reproducible current model after missing preferences", () => {
		const current = getModel("openai", "gpt-4o");
		if (!current) throw new Error("Missing test model");
		const child = { ...current };
		const result = resolveModel(
			{ ...createAgentConfig(), parsedModels: [selector("missing/model")] },
			{
				model: current,
				availableModels: [child],
			},
		);
		expect(result.model).toBe(child);
		expect(result.usedCurrentModel).toBe(true);
		expect(result.warnings).toHaveLength(2);
	});

	it("selects an available preference before an unavailable current model", () => {
		const current = getModel("anthropic", "claude-sonnet-4-5");
		const preferred = getModel("openai", "gpt-4o");
		if (!current || !preferred) throw new Error("Missing test model");
		const result = resolveModel(
			{
				...createAgentConfig(),
				parsedModels: [selector(`${preferred.provider}/${preferred.id}`)],
			},
			{
				model: current,
				availableModels: [preferred],
			},
		);
		expect(result.model).toBe(preferred);
		expect(result.usedCurrentModel).toBe(false);
	});

	it("uses the matching child model rather than the parent object", () => {
		const current = getModel("openai", "gpt-4o");
		if (!current) throw new Error("Missing test model");
		const child = { ...current, baseUrl: "https://child.invalid" };
		expect(
			resolveModel(createAgentConfig(), { model: current, availableModels: [child] }).model,
		).toBe(child);
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
