import { z } from "zod";
import { parseYamlConfig, type RawConfig } from "../_shared/config";
import { parseModel, parseModelPreferences } from "./loader/utils";
import type { AgentConfigFields, ParsedModel } from "./loader/types";

export type TeamConfig = {
	agents: Record<string, AgentConfigFields>;
};

const thinkingLevelSchema = z.enum([
	"minimal",
	"low",
	"medium",
	"high",
	"xhigh",
]);

const modelPreferencesSchema = z.union([
	z.string(),
	z.array(z.string()),
]);

const parseModelFields = (
	model: string | string[] | undefined,
): Pick<AgentConfigFields, "model" | "parsedModel" | "parsedModels"> => {
	const modelPreferences = parseModelPreferences(model);
	if (!modelPreferences || modelPreferences.length === 0) {
		return {};
	}

	const parsedModels: ParsedModel[] = [];
	for (const candidate of modelPreferences) {
		const parsed = parseModel(candidate);
		if (parsed) {
			parsedModels.push(parsed);
		}
	}

	const normalizedModel =
		modelPreferences.length === 1 ? modelPreferences[0] : modelPreferences;

	return {
		model: normalizedModel,
		parsedModel: parsedModels[0],
		parsedModels,
	};
};

const agentConfigFieldsSchema = z
	.object({
		model: modelPreferencesSchema.optional(),
		thinking: thinkingLevelSchema.optional(),
		tools: z.array(z.string()).optional(),
		skills: z.array(z.string()).optional(),
		compaction: z.boolean().optional(),
		interactive: z.boolean().optional(),
	})
	.transform((config): AgentConfigFields => ({
		...config,
		...parseModelFields(config.model),
	}));

const teamConfigSchema = z.object({
	agents: z.record(agentConfigFieldsSchema).default({}),
});

export function normalizeTeamConfig(raw: RawConfig): TeamConfig {
	return teamConfigSchema.parse(raw);
}

export const loadTeamConfig = (content: string): TeamConfig =>
	normalizeTeamConfig(parseYamlConfig(content));
