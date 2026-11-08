import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { getAgentDir } from "@earendil-works/pi-coding-agent";
import Type from "typebox";
import { discoverAgents } from "../catalog";
import { canSpawnSubagent } from "../delegationPolicy";
import type { Runtime } from "../runtime";
import { getToolContext, toolError, toolSuccess } from "./shared";

export const teamSpawnToolMetadata = {
	description:
		"Spawn a non-blocking subagent in an isolated session. team_spawn reloads configuration and enforces agent existence and delegation permission before asynchronous runner startup. During startup, model and tool resolution may fall back and emit warnings. Results return as steering messages.",
	subagentParameterDescription:
		"Known permitted subagent name; root may use its injected routing roster, or use a name discovered with team_list",
	briefParameterDescription:
		"Concise task label for session lists, ideally under 80 characters. This is not the full task.",
	taskParameterDescription: "Task to delegate to the subagent",
	promptSnippet:
		"Spawn a non-blocking subagent directly by a known permitted name. Root may use its injected routing roster; team_list is not required first.",
	promptGuidelines: [
		"team_spawn: Spawn directly by a known permitted name. For root, the injected routing roster is one source; do not call team_list first solely to authorize a spawn.",
		"team_spawn: Reloads configuration and enforces agent existence and delegation permission before asynchronous runner startup. During startup, model and tool resolution may fall back and emit warnings.",
		"team_spawn: Use team_list only when you need discovery, declared capability detail, or your caller-owned active status to avoid duplicate work; its snapshot does not guarantee availability or freshness.",
		"team_spawn: Keep brief short; put necessary context and criteria in task.",
		"team_spawn: Do not duplicate delegated work. Continue useful independent work; otherwise end your turn and await steering results.",
		"team_spawn: Do not sleep, repeatedly call team_list, or make placeholder tool calls solely to wait; a one-off status check when needed is fine.",
	],
};

export const registerTeamSpawnTool = (pi: ExtensionAPI, runtime: Runtime): void => {
	pi.registerTool({
		name: "team_spawn",
		label: "Spawn team agent",
		description: teamSpawnToolMetadata.description,
		parameters: Type.Object({
			subagent: Type.String({ description: teamSpawnToolMetadata.subagentParameterDescription }),
			brief: Type.String({ description: teamSpawnToolMetadata.briefParameterDescription }),
			task: Type.String({ description: teamSpawnToolMetadata.taskParameterDescription }),
		}),
		promptSnippet: teamSpawnToolMetadata.promptSnippet,
		promptGuidelines: teamSpawnToolMetadata.promptGuidelines,
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
				const availableAgents = catalog.agents.map((agent) => agent.name).join(", ") || "none";
				return toolError(
					`Agent "${params.subagent}" not found. Available agents: ${availableAgents}`,
				);
			}

			const caller = runtime.getSessionAgent(toolCtx.callerSessionId);
			if (!canSpawnSubagent(caller, subagent)) {
				return toolError(
					`Agent "${caller?.name ?? "root"}" is not permitted to spawn "${subagent.name}"`,
				);
			}

			const id = runtime.spawn(subagent, params.task, toolCtx.cwd, toolCtx.callerSessionId, {
				brief,
				model: ctx.model,
				modelRegistry: ctx.modelRegistry,
				agentDir: getAgentDir(),
				parentSessionFile: ctx.sessionManager.getSessionFile(),
				availableToolNames: pi.getAllTools().map((tool) => tool.name),
				onWarning: (message) => {
					runtime.reportWarning(toolCtx.callerSessionId, message);
				},
			});

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
