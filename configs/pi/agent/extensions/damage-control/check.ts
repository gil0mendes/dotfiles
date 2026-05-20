import type { Action, Rule, Safety } from "./types";

/**
 * Check if an action is safe according to the given rules.
 */
export function checkAction<TMeta = null>(
	action: Action,
	rules: readonly Rule<TMeta>[],
): Safety<TMeta> {
	for (const rule of rules) {
		const result = rule.check(action);

		if (result.kind === "match") {
			return {
				kind: "dangerous",
				action,
				key: rule.key,
				reason: result.reason,
				metadata: result.metadata,
			};
		}
	}

	return { kind: "safe" };
}
