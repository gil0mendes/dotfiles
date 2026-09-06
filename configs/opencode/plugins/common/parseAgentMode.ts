import type { Logger } from "./logger";
import type { OpencodeClient } from "./types";

/**
 * Parse agent mode at boundary.
 * Returns trusted type indicating if agent is a sub-agent.
 */
export async function parseAgentMode(
	client: OpencodeClient,
	agentName: string,
	log: Logger,
): Promise<{ isSubAgent: boolean }> {
	try {
		const agents = await client.agent.list();
		const agent = agents.data.find((candidate) => candidate.id === agentName);

		return {
			isSubAgent: agent?.mode === "subagent",
		};
	} catch (error) {
		// Fail-safe: Agent list errors shouldn't block task calls
		// Fail-loud: Log for observability
		log.warn(
			`Agent list fetch failed for "${agentName}", assuming non-sub-agent: ${error instanceof Error ? error.message : String(error)}`,
		);
		return { isSubAgent: false };
	}
}
