import * as os from "node:os";
import * as path from "node:path";
import type { Rule } from "./types";

export function expandHome(p: string): string {
	return p === "~" || p.startsWith("~/")
		? path.join(os.homedir(), p.slice(2))
		: p;
}

export function absoluteTarget(p: string, cwd: string): string {
	return path.resolve(cwd, expandHome(p));
}

function globToRegex(glob: string): RegExp {
	const expanded = expandHome(glob).replace(/\/+$|\\+$/g, "");
	const escaped = expanded
		.replace(/[.+^${}()|[\]\\]/g, "\\$&")
		.replace(/\*/g, "[^/]*");
	return new RegExp(`(^|/)${escaped}($|/)`);
}

function pathMatches(target: string, rulePath: string, cwd: string): boolean {
	const absTarget = absoluteTarget(target, cwd).replace(/\/+$|\\+$/g, "");
	const relTarget = path.relative(cwd, absTarget) || ".";
	const expandedRule = expandHome(rulePath);

	if (expandedRule.endsWith("/")) {
		const absRule = (
			path.isAbsolute(expandedRule)
				? expandedRule
				: path.resolve(cwd, expandedRule)
		).replace(/\/+$|\\+$/g, "");
		return absTarget === absRule || absTarget.startsWith(absRule + path.sep);
	}

	const regex = globToRegex(expandedRule);
	return regex.test(absTarget) || regex.test(relTarget);
}

export function ruleValue(rule: Rule): string {
	return rule.pattern ?? rule.path ?? rule.command ?? "";
}

export function matchPathRules(
	targets: string[],
	rules: Rule[],
	cwd: string,
): Rule | null {
	for (const target of targets) {
		for (const rule of rules) {
			const value = ruleValue(rule);
			if (value && pathMatches(target, value, cwd)) return rule;
		}
	}
	return null;
}

export function matchRegexRules(text: string, rules: Rule[]): Rule | null {
	for (const rule of rules) {
		const value = ruleValue(rule);
		if (!value) continue;
		try {
			if (new RegExp(value, "im").test(text)) return rule;
		} catch {
			if (text.includes(value)) return rule;
		}
	}
	return null;
}

export function matchLiteralPathInCommand(
	command: string,
	rules: Rule[],
): Rule | null {
	for (const rule of rules) {
		const value = ruleValue(rule);
		if (!value) continue;
		const bare = value
			.replace(/^~\//, "")
			.replace(/\/+$|\\+$/g, "")
			.replace(/^\*+/, "")
			.replace(/\*+$/, "");
		if (bare && command.includes(bare)) return rule;
	}
	return null;
}
