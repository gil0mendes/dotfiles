import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { discoverAgents } from "./catalog";
import { getPermittedSubagents } from "./delegationPolicy";
import type { Runtime } from "./runtime";

type BuildSystemPromptArgs = {
	agents: string;
};

const buildSystemPrompt = ({ agents }: BuildSystemPromptArgs) => `
<system-reminder>
	<workflow-routing policy_level="critical">
		You are an orchestrator that coordinates the work of multiple agents to solve complex problems.
		You should avoid doing any work by yourself, like reading files, doing tool calls, etc., and instead delegate to the agents below.

   	## Agent Routing (STRICT BOUNDARIES)
    The following agents are available for you to call:
    <agents>
    	${agents}
    </agents>
  </workflow-routing>
</system-reminder>`;

export const registerSystemPrompt = (pi: ExtensionAPI, runtime: Runtime) => {
	pi.on("before_agent_start", (event, ctx) => {
		const sessionId = ctx.sessionManager.getSessionId();
		const caller = runtime.getSessionAgent(sessionId);
		if (caller) return;

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
		const systemPrompt = buildSystemPrompt({ agents });

		return {
			systemPrompt: `${event.systemPrompt}\n${systemPrompt}`,
		};
	});
};
