import type { OpencodeClient } from "@opencode-ai/sdk";
import type { Logger } from "./logger";

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
		const agents = (result.data ?? []) as { name: string; mode?: string }[];
		const agent = agents.find((agent) => agent.name === agentName);
		return { isSubAgent: agent?.mode === "subagent" };
	} catch (error) {
		// Fail-safe: Agent list errors shouldn't block task calls
		// Fail-loud: Log for observability
		log.warn(
			`Agent list fetch failed for "${agentName}", assuming non-sub-agent: ${error instanceof Error ? error.message : String(error)}`,
		);
		return { isSubAgent: false };
	}
}
