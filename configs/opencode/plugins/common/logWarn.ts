/**
 * Provides a unified interface for logging warnings that works with
 * both the OpenCode client (when available) and console fallback.
 */

/**
 * Log a warning message via OpenCode client or console fallback.
 *
 * Uses the OpenCode logging API when a client is available, which integrates
 * with the OpenCode UI log panel. Falls back to console.warn for CLI contexts
 * or when no client is provided.
 *
 * @param client - Optional OpenCode client for proper logging integration
 * @param service - Service name for log categorization (e.g., "worktree", "delegation")
 * @param message - Warning message to log
 *
 * @example
 * ```ts
 * // With client - logs to OpenCode UI
 * logWarn(client, "delegation", "Task timed out after 30s")
 *
 * // Without client - logs to console
 * logWarn(undefined, "delegation", "Task timed out after 30s")
 * ```
 */
export function logWarn(
	service: string,
	message: string,
): void {
	console.warn(`[${service}] ${message}`);
}
