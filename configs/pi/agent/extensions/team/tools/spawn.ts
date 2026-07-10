import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { getAgentDir } from "@earendil-works/pi-coding-agent";
import Type from "typebox";
import { discoverAgents } from "../catalog";
import type { Runtime } from "../runtime";
import { getToolContext, toolError, toolSuccess } from "./shared";

export const registerTeamSpawnTool = (
	pi: ExtensionAPI,
	runtime: Runtime,
): void => {
	pi.registerTool({
		name: "team_spawn",
		label: "Spawn team agent",
		description:
			"Spawn a non-blocking subagent that runs in an isolated session. The subagent works independently while your session stays interactive. Results are delivered back to your session as steering messages.",
		parameters: Type.Object({
			subagent: Type.String({ description: "Subagent name from team_list" }),
			brief: Type.String({
				description:
					"Concise task label for session lists, ideally under 80 characters. This is not the full task.",
			}),
			task: Type.String({ description: "Task to delegate to the subagent" }),
		}),
		promptSnippet:
			"Spawn a non-blocking subagent. Use team_list first to see available subagents.",
		promptGuidelines: [
			"team_spawn: Use only after team_list, for one bounded self-contained task.",
			"team_spawn: Keep brief short; put necessary context and criteria in task.",
			"team_spawn: Do not duplicate delegated work; wait for steering results.",
		],
		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const toolCtx = getToolContext(ctx);
			const catalog = discoverAgents(toolCtx.cwd);

			const brief = params.brief.trim();
			if (!brief) {
				return toolError("Brief is required and must not be empty");
			}

			for (const warning of catalog.warnings) {
				runtime.reportWarning(toolCtx.callerSessionId, warning.message);
			}

			const subagent = catalog.getAgent(params.subagent);
			if (!subagent) {
				const availableAgents =
					catalog.agents.map((agent) => agent.name).join(", ") || "none";
				return toolError(
					`Agent "${params.subagent}" not found. Available agents: ${availableAgents}`,
				);
			}

			const id = runtime.spawn(
				subagent,
				params.task,
				toolCtx.cwd,
				toolCtx.callerSessionId,
				{
					brief,
					model: ctx.model,
					modelRegistry: ctx.modelRegistry,
					agentDir: getAgentDir(),
					parentSessionFile: ctx.sessionManager.getSessionFile(),
					onWarning: (message) => {
						runtime.reportWarning(toolCtx.callerSessionId, message);
					},
				},
			);

			return toolSuccess(
				`Subagent "${subagent.name}" spawned as ${id}. Result will be delivered via steering messages.`,
				{
					id,
					agentName: subagent.name,
					brief,
					task: params.task,
				},
			);
		},
	});
};
