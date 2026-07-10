import type { ThinkingLevel } from "@earendil-works/pi-ai";

export type ParsedModel = {
	provider: string;
	modelId: string;
};

export type AgentConfigFields = {
	model?: string | string[];
	parsedModel?: ParsedModel;
	parsedModels?: ParsedModel[];
	thinking?: ThinkingLevel;
	// TODO: validate tools against available tool names
	tools?: string[];
	skills?: string[];
	compaction?: boolean;
	interactive?: boolean;
};

export type AgentConfig = AgentConfigFields & {
	name: string;
	description: string;
	systemPrompt: string;
	filePath: string;
};
