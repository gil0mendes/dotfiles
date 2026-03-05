import type { Logger } from "./logger";
import type { OpencodeClient } from "./types";

/**
 * Permission entry type: simple value or pattern object.
 * Matches CLI schema: z.union([z.enum(["ask", "allow", "deny"]), z.record(z.enum(...))])
 */
type PermissionEntry =
	| "ask"
	| "allow"
	| "deny"
	| Record<string, "ask" | "allow" | "deny">;

/**
 * Check if a permission entry denies access (Law 4: Fail Fast).
 * Handles both simple values ("deny") and pattern objects ({ "*": "deny" }).
 */
function isPermissionDenied(entry: PermissionEntry | undefined): boolean {
	if (entry === undefined) return false;
	if (entry === "deny") return true;
	if (typeof entry === "object" && entry["*"] === "deny") return true;
	return false;
}

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
		const config = await client.config.get();
		const configData = config.data as {
			agent?: Record<
				string,
				{
					permission?: Record<string, PermissionEntry>;
				}
			>;
		};
		const permission = configData?.agent?.[agentName]?.permission ?? {};

		const editDenied = isPermissionDenied(permission.edit);
		const writeDenied = isPermissionDenied(permission.write);
		const bashDenied = isPermissionDenied(permission.bash);

		return { isReadOnly: editDenied && writeDenied && bashDenied };
	} catch (error) {
		// Fail-safe: Config errors shouldn't block task calls
		// Fail-loud: Log for observability
		log.warn(
			`Config fetch failed for "${agentName}", assuming write-capable: ${error instanceof Error ? error.message : String(error)}`,
		);
		return { isReadOnly: false };
	}
}
