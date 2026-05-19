import { checkDangerousCommand } from "../commands/dangerous";
import { matchRegexRules } from "../rules";
import type { Config, PatternConfig } from "../types";
import { stripHeredocs } from "./ast";

export function evaluateBash(
	command: string,
	config: Config,
): { rule?: PatternConfig; reason: string; ask: boolean } | null {
	if (config.permissionGate === false) return null;

	const cmd = stripHeredocs(command);
	const explicit = matchRegexRules(cmd, config.bashToolPatterns);
	if (explicit)
		return {
			rule: explicit,
			reason: explicit.reason ?? "dangerous bash command",
			ask: explicit.ask === true,
		};

	if (config.useBuiltinCommandMatchers !== false) {
		const dangerous = checkDangerousCommand({
			command: cmd,
			patterns: [],
			useBuiltinMatchers: true,
			fallbackPatterns: [],
		});
		if (dangerous)
			return {
				reason: dangerous.description,
				ask: false,
			};
	}

	if (config.strictMode) {
		const allowed = matchRegexRules(cmd, config.strictModeAllowedList);
		if (!allowed)
			return {
				reason:
					"strict mode blocks bash commands not matching strictModeAllowedList",
				ask: false,
			};
	}

	return null;
}
