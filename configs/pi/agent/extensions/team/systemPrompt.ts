import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { discoverAgents } from "./catalog";
import { getPermittedSubagents } from "./delegationPolicy";
import type { Runtime } from "./runtime";

type BuildSystemPromptArgs = {
	agents: string;
};

export const buildRootSystemPrompt = ({ agents }: BuildSystemPromptArgs): string => `
<system-reminder>
	<workflow-routing policy_level="critical">
		You are the root orchestrator coordinating multiple agents for complex work.
		Avoid doing the work yourself; delegate it to the agents below.

		## Root Agent Routing (STRICT BOUNDARIES)
		This injected roster is a routing snapshot. You may call team_spawn directly with the name of any listed permitted agent; team_list is not required first.
		team_spawn reloads configuration and enforces agent existence and delegation permission before asynchronous runner startup. During startup, model and tool resolution may fall back and emit warnings.
		Use team_list only when you need discovery, declared capability detail, or your caller-owned active status and duplicate-work check. Its snapshot does not guarantee availability or freshness.
		<agents>
			${agents}
		</agents>
	</workflow-routing>
</system-reminder>`;

export const createBeforeAgentStartHandler =
	(runtime: Pick<Runtime, "getSessionAgent">) =>
	(
		event: { systemPrompt: string },
		ctx: { cwd: string; sessionManager: { getSessionId(): string } },
	) => {
		const sessionId = ctx.sessionManager.getSessionId();
		const caller = runtime.getSessionAgent(sessionId);
		if (caller !== undefined) return;

		const catalog = discoverAgents(ctx.cwd);
		const agents = getPermittedSubagents(caller, catalog.agents)
			.map(
				(agent) =>
					`<agent>
						<name>${agent.name}</name>
						<description>${agent.description}</description>
					</agent>`,
			)
			.join("\n");
		const systemPrompt = buildRootSystemPrompt({ agents });

		return {
			systemPrompt: `${event.systemPrompt}\n${systemPrompt}`,
		};
	};

export const registerSystemPrompt = (pi: ExtensionAPI, runtime: Runtime) => {
	pi.on("before_agent_start", createBeforeAgentStartHandler(runtime));
};
