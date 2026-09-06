import type { Context } from "@opencode/plugin/promise/plugin";

/**
 * OpenCode client instance type.
 *
 * Derived from the plugin input client type for consistency.
 */
export type OpencodeClient = Pick<Context, "agent" | "generate" | "session">;
