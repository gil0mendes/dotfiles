/**
 * Create a logger for plugin diagnostics.
 */
export function createLogger() {
	const log = (level: "debug" | "info" | "warn" | "error", message: string) =>
		console[level](`[background-agents] ${message}`);

	return {
		debug: (msg: string) => log("debug", msg),
		info: (msg: string) => log("info", msg),
		warn: (msg: string) => log("warn", msg),
		error: (msg: string) => log("error", msg),
	};
}

export type Logger = ReturnType<typeof createLogger>;
