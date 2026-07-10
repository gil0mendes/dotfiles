import { loadTeamConfig } from "./config";
import {
	FileSystemAgentCatalogSource,
	loadAgentDefinitionFromFile,
	type AgentCatalogSource,
	type AgentDefinitionFile,
	type AgentDefinitionSourceGroup,
} from "./loader/loadAgents";
import type { AgentConfig, AgentConfigFields } from "./loader/types";
import {
	createDiscoveryWarning,
	type AgentDiscoveryWarning,
} from "./loader/utils";

export type AgentDiscoveryResult = {
	agents: AgentConfig[];
	warnings: AgentDiscoveryWarning[];
};

type ConfigParseResult = {
	overrides: Record<string, AgentConfigFields>;
	overridesSources: Record<string, string>;
	warnings: AgentDiscoveryWarning[];
};

const mergeConfigFiles = (
	configFiles: AgentDefinitionFile[],
): ConfigParseResult => {
	const warnings: AgentDiscoveryWarning[] = [];
	const overrides: Record<string, AgentConfigFields> = {};
	const overridesSources: Record<string, string> = {};

	for (const configFile of configFiles) {
		warnings.push(...(configFile.warnings ?? []));

		if (!configFile.content) {
			continue;
		}

		try {
			const parsed = loadTeamConfig(configFile.content);
			for (const [name, config] of Object.entries(parsed.agents)) {
				overrides[name] = config;
				overridesSources[name] = configFile.filePath;
			}
		} catch (error) {
			const reason = error instanceof Error ? error.message : String(error);
			warnings.push(
				createDiscoveryWarning(
					configFile.filePath,
					`Failed to parse team config: ${reason}`,
				),
			);
		}
	}

	return {
		overrides,
		overridesSources,
		warnings,
	};
};

class AgentCatalog {
	public agents: AgentConfig[] = [];
	public warnings: AgentDiscoveryWarning[] = [];
	private seenNames: Map<string, string> = new Map();

	constructor(private readonly source: AgentCatalogSource) {}

	discover(cwd: string = process.cwd()): AgentDiscoveryResult {
		// load agent definitions from all the groups
		const groups = this.source.loadAgentDefinitionGroups(cwd);
		for (const group of groups) {
			this.loadAgentsFromGroup(group);
		}

		// load optional team.yaml config overrides
		const configOverrides = mergeConfigFiles(this.source.loadConfigFiles(cwd));
		this.warnings.push(...configOverrides.warnings);

		this.agents = this.agents.map((agent) => ({
			...agent,
			...configOverrides.overrides[agent.name],
		}));

		return { agents: this.agents, warnings: this.warnings };
	}

	private loadAgentsFromGroup(group: AgentDefinitionSourceGroup): void {
		this.warnings.push(...(group.warnings ?? []));
		const groupNames = new Set<string>();

		for (const file of group.files) {
			const loaded = loadAgentDefinitionFromFile(file);
			this.warnings.push(...loaded.warnings);
			if (!loaded.agent) {
				continue;
			}

			const { name } = loaded.agent;
			if (groupNames.has(name)) {
				this.warnings.push(
					createDiscoveryWarning(
						file.filePath,
						`Agent name "${name}" is duplicated in ${group.agentsDir}`,
					),
				);
				continue;
			}

			groupNames.add(name);
			if (this.seenNames.has(name)) {
				continue;
			}

			this.seenNames.set(name, file.filePath);
			this.agents.push(loaded.agent);
		}
	}

	public getAgent(name: string): AgentConfig | undefined {
		return this.agents.find((agent) => agent.name === name);
	}
}

export const discoverAgents = (cwd: string): AgentCatalog => {
	const fileSystemCatalog = new FileSystemAgentCatalogSource();
	const catalog = new AgentCatalog(fileSystemCatalog);
	catalog.discover(cwd);
	return catalog;
};
