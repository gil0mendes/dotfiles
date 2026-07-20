import type { ThinkingLevel } from "@earendil-works/pi-ai";

export type ParsedModel = {
	provider: string;
	modelId: string;
};

export type AgentType = "orchestrator" | "agent";

export type AgentConfigFields = {
	model?: string | string[];
	type?: AgentType;
	parsedModel?: ParsedModel;
	parsedModels?: ParsedModel[];
	thinking?: ThinkingLevel;
	tools?: string[];
	skills?: string[];
	compaction?: boolean;
	interactive?: boolean;
};

export type AgentConfig = Omit<AgentConfigFields, "type"> & {
	name: string;
	type: AgentType;
	description: string;
	systemPrompt: string;
	filePath: string;
};
