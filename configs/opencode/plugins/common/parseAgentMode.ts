import type { Logger } from "./logger";
import type { OpencodeClient } from "./types";

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null;
}

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
		const result = await client.app.agents({});
		const rawAgents = result.data;
		const agents = Array.isArray(rawAgents) ? rawAgents : [];
		const agent = agents.find(
			(candidate) =>
				isRecord(candidate) &&
				typeof candidate.name === "string" &&
				candidate.name === agentName,
		);

		return {
			isSubAgent:
				isRecord(agent) &&
				typeof agent.mode === "string" &&
				agent.mode === "subagent",
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
