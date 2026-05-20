import type { BashDangerousPattern, Rule, Action } from "../types";
import { checkDangerousCommand, compileCommandPatterns } from "./dangerous";

const PERMISSION_GATE_KEY = "permission-gate.dangerous-command";

export type PermissionGateMeta = {
	command: string;
	description: string;
	pattern: string;
};

export type PermissionGateRuleOptions = {
	patterns: BashDangerousPattern[];
	useBuiltinMatchers: boolean;
};

export function createPermissionGateRule({
	patterns,
	useBuiltinMatchers,
}: PermissionGateRuleOptions): Rule<PermissionGateMeta> {
	const compiledPatterns = compileCommandPatterns(patterns);

	return {
		key: PERMISSION_GATE_KEY,
		check(action: Action) {
			if (action.kind !== "command") return { kind: "pass" };

			const match = checkDangerousCommand({
				command: action.command,
				patterns: compiledPatterns,
				useBuiltinMatchers,
				fallbackPatterns: patterns,
			});

			if (!match) return { kind: "pass" };

			return {
				kind: "match",
				reason: match.description,
				metadata: {
					command: action.command,
					description: match.description,
					pattern: match.pattern,
				},
			};
		},
	};
}
