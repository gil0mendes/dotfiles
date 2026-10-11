import type { AgentConfig } from "./loader/types";

export const getPermittedSubagents = (
	caller: AgentConfig | undefined,
	availableAgents: readonly AgentConfig[],
): AgentConfig[] => {
	if (!caller) return [...availableAgents];

	const permittedNames = new Set(caller.delegatesTo ?? []);
	return availableAgents.filter(
		(agent) =>
			agent.name !== caller.name &&
			!agent.rootOnly &&
			permittedNames.has(agent.name),
	);
};

export const canSpawnSubagent = (
	caller: AgentConfig | undefined,
	subagent: AgentConfig,
): boolean => getPermittedSubagents(caller, [subagent]).length === 1;
