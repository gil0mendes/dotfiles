import { z } from "zod";
import { parseYamlConfig, type RawConfig } from "../_shared/config";
import { parseModelDeclaration, type ModelDeclarationIssue } from "./loader/utils";
import type { AgentConfigFields, ParsedModel } from "./loader/types";

export type TeamConfigWarning = {
	agentName: string;
	issue: ModelDeclarationIssue;
};

export type TeamConfig = {
	agents: Record<string, AgentConfigFields>;
	warnings: TeamConfigWarning[];
};

const thinkingLevelSchema = z.enum(["minimal", "low", "medium", "high", "xhigh"]);

const agentConfigFieldsSchema = z.object({
	model: z.unknown().optional(),
	thinking: thinkingLevelSchema.optional(),
	tools: z.array(z.string()).optional(),
	skills: z.array(z.string()).optional(),
	compaction: z.boolean().optional(),
	interactive: z.boolean().optional(),
	delegatesTo: z.array(z.string()).optional(),
	rootOnly: z.boolean().optional(),
});

const teamConfigSchema = z.object({
	agents: z.record(z.string(), agentConfigFieldsSchema).default({}),
});

const parseModelFields = (
	model: unknown,
): {
	fields: Pick<AgentConfigFields, "model" | "parsedModel" | "parsedModels">;
	issues: ModelDeclarationIssue[];
} => {
	if (model === undefined) return { fields: {}, issues: [] };

	const parsed = parseModelDeclaration(model);
	const parsedModels: ParsedModel[] = parsed.parsedModels;
	const firstModel = parsedModels[0];
	return {
		fields: {
			...(parsed.model === undefined ? {} : { model: parsed.model }),
			...(firstModel === undefined ? {} : { parsedModel: firstModel, parsedModels }),
		},
		issues: parsed.issues,
	};
};

export function normalizeTeamConfig(raw: RawConfig): TeamConfig {
	const parsed = teamConfigSchema.parse(raw);
	const agents: Record<string, AgentConfigFields> = {};
	const warnings: TeamConfigWarning[] = [];

	for (const [agentName, config] of Object.entries(parsed.agents)) {
		const { model, ...otherFields } = config;
		const parsedModel = parseModelFields(model);
		agents[agentName] = { ...otherFields, ...parsedModel.fields };
		for (const issue of parsedModel.issues) {
			warnings.push({ agentName, issue });
		}
	}

	return { agents, warnings };
}

export const loadTeamConfig = (content: string): TeamConfig =>
	normalizeTeamConfig(parseYamlConfig(content));
