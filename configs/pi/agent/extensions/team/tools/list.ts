import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { CONFIG_DIR_NAME, getAgentDir } from "@earendil-works/pi-coding-agent";
import Type from "typebox";
import { discoverAgents } from "../catalog";
import type { AgentConfig } from "../loader/types";
import type { AgentDiscoveryWarning } from "../loader/utils";
import type { ActiveAgentSummary, Runtime } from "../runtime";
import { getToolContext, toolSuccess } from "./shared";

const STATUS_ICON: Record<ActiveAgentSummary["status"], string> = {
	running: "🏃",
	waiting: "⏳",
	done: "✅",
	error: "❌",
	aborted: "🛑",
};

const formatAvailableAgents = (agents: AgentConfig[]): string[] => {
	if (agents.length === 0) {
		return [
			`No valid subagent definitions found. Add \`.md\` files to \`<cwd>/${CONFIG_DIR_NAME}/agents/\` or \`${getAgentDir()}/agents/\`.`,
		];
	}

	return agents.flatMap((agent) => {
		const tools =
			agent.tools === undefined
				? "all built-in"
				: agent.tools.length === 0
					? "none"
					: agent.tools.join(", ");
		const skills =
			agent.skills === undefined
				? "all built-in"
				: agent.skills.length === 0
					? "none"
					: agent.skills.join(", ");
		return [
			"",
			`name: ${agent.name}`,
			`description: ${agent.description}`,
			`interactive: ${agent.interactive ? "true" : "false"}`,
			`tools: ${tools}`,
			`skills: ${skills}`,
		];
	});
};

const formatWarnings = (warnings: AgentDiscoveryWarning[]): string[] => {
	if (warnings.length === 0) return [];
	return [
		"",
		"## Ignored subagent definitions",
		...warnings.map((warning) => `- ${warning.message} (${warning.filePath})`),
	];
};

const formatActiveAgents = (running: ActiveAgentSummary[]): string[] => {
	if (running.length === 0) return ["No subagents currently active."];
	return running.flatMap((agent) => {
		const icon = STATUS_ICON[agent.status] ?? "❓";
		return [
			"",
			`id: ${agent.id}`,
			`name: ${agent.agentName}`,
			`status: ${icon} ${agent.status}`,
		];
	});
};

export const registerTeamListTool = (
	pi: ExtensionAPI,
	runtime: Runtime,
): void => {
	pi.registerTool({
		name: "team_list",
		label: "List team",
		description: "List subagent definitions and active subagents",
		parameters: Type.Object({}),
		promptSnippet: "List available subagents and active subagents.",
		promptGuidelines: [
			"team_list: Use for discovery or a requested one-time status snapshot.",
		],
		execute: async (_toolCallId, _params, _signal, _onUpdate, ctx) => {
			const toolCtx = getToolContext(ctx);
			const catalog = discoverAgents(toolCtx.cwd);
			const runningAgents = runtime.getActiveSummariesForOwner(
				toolCtx.callerSessionId,
			);

			const lines = [
				"## Available Subagents",
				...formatAvailableAgents(catalog.agents),
				...formatWarnings(catalog.warnings),
				"",
				"## Active Subagents",
				...formatActiveAgents(runningAgents),
			];


			return toolSuccess(lines.join("\n"));
		},
	});
};
