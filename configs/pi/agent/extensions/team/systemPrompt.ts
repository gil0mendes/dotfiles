import { type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { discoverAgents } from "./catalog";
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
	const catalog = discoverAgents(runtime.getCwd());
	const agents = catalog.agents
		.filter((agent) => agent.type === "orchestrator")
		.map(
			(agent) =>
				`<agent>
					<name>${agent.name}</name>
					<description>${agent.description}</description>
				</agent>`,
		)
		.join("\n");
	const systemPrompt = buildSystemPrompt({ agents });

	pi.on("before_agent_start", (event) => {
		return {
			systemPrompt: `${event.systemPrompt}\n${systemPrompt}`,
		};
	});
};
