import type { ThinkingLevel } from "@earendil-works/pi-ai";

export type ParsedModel =
	| { kind: "qualified"; raw: string; provider: string; modelId: string }
	| { kind: "bare"; raw: string; query: string };

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
	delegatesTo?: string[];
	rootOnly?: boolean;
};

export type AgentConfig = Omit<AgentConfigFields, "type"> & {
	name: string;
	type: AgentType;
	description: string;
	systemPrompt: string;
	filePath: string;
};
