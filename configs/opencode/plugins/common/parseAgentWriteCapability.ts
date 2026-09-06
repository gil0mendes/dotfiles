import type { Logger } from "./logger";
import type { OpencodeClient } from "./types";

/**
 * Parse agent write capability at boundary.
 * Returns trusted type indicating if agent is read-only.
 *
 * An agent is read-only when ALL of: edit, write, and bash are denied.
 * Permission schema supports both simple ("deny") and pattern ({ "*": "deny" }) values.
 */
export async function parseAgentWriteCapability(
	client: OpencodeClient,
	agentName: string,
	log: Logger,
): Promise<{ isReadOnly: boolean }> {
	try {
		const agent = await client.agent.get({ agentID: agentName });
		const permissions = agent.data.permissions;
		const lastEffect = (action: string): "allow" | "ask" | "deny" | undefined => {
			let effect: "allow" | "ask" | "deny" | undefined;
			for (const rule of permissions) {
				if (rule.resource !== "*") continue;
				if (rule.action !== "*" && rule.action !== action) continue;
				effect = rule.effect;
			}
			return effect;
		};

		return {
			isReadOnly:
				lastEffect("edit") === "deny" && lastEffect("shell") === "deny",
		};
	} catch (error) {
		// Fail-safe: Config errors shouldn't block task calls
		// Fail-loud: Log for observability
		log.warn(
			`Config fetch failed for "${agentName}", assuming write-capable: ${error instanceof Error ? error.message : String(error)}`,
		);
		return { isReadOnly: false };
	}
}
