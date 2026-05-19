import type { Program, SimpleCommand, Word } from "@aliou/sh";
import * as fs from "node:fs";
import * as path from "node:path";
import { checkDangerousCommand } from "../commands/dangerous";
import { isInsideOrSame } from "../paths";
import { absoluteTarget, matchLiteralPathInCommand, matchRegexRules, ruleValue } from "../rules";
import type { Config, Rule } from "../types";
import { parseShell, stripHeredocs, walkCommands, walkStatements, wordToString } from "./ast";

function commandLooksMutating(command: string): boolean {
	return (
		/(^|[;&|]\s*)(rm|rmdir|mv|cp|install|touch|mkdir|chmod|chown|truncate|tee|sed\s+-i|perl\s+-pi|python\b|node\b|bun\b|npm\b|pnpm\b|yarn\b|git\s+(clean|reset|checkout|restore|branch|push)|cat\s*>|echo\s+.*>|printf\s+.*>)\b/im.test(
			command,
		) || />{1,2}/.test(command)
	);
}

function commandLooksDeleting(command: string): boolean {
	return /(^|[;&|]\s*)(rm|rmdir|unlink|shred|trash|mv|git\s+(clean|branch\s+(-\S*)*D|push\s+\S+\s+--delete))\b/im.test(
		command,
	);
}

function splitCommandWords(command: string): string[] {
	const words: string[] = [];
	let current = "";
	let quote: string | null = null;
	let escaped = false;

	for (const ch of command) {
		if (escaped) {
			current += ch;
			escaped = false;
			continue;
		}
		if (ch === "\\") {
			escaped = true;
			continue;
		}
		if (quote) {
			if (ch === quote) quote = null;
			else current += ch;
			continue;
		}
		if (ch === '"' || ch === "'") {
			quote = ch;
			continue;
		}
		if (/\s|[;&|()]/.test(ch)) {
			if (current) words.push(current);
			current = "";
			continue;
		}
		current += ch;
	}
	if (current) words.push(current);
	return words;
}

function commandWords(cmd: SimpleCommand): string[] {
	return (cmd.words ?? []).map(wordToString);
}

function commandBasename(command: string): string {
	return path.basename(command).toLowerCase();
}

function commandName(cmd: SimpleCommand): string | null {
	const [first] = commandWords(cmd);
	return first ? commandBasename(first) : null;
}

function astCommandLooksMutating(program: Program): boolean {
	let mutating = false;
	walkCommands(program, (cmd) => {
		const name = commandName(cmd);
		const words = commandWords(cmd);
		const args = words.slice(1);
		if (!name) return;
		if (
			[
				"rm",
				"rmdir",
				"mv",
				"cp",
				"install",
				"touch",
				"mkdir",
				"chmod",
				"chown",
				"truncate",
				"tee",
				"python",
				"node",
				"bun",
				"npm",
				"pnpm",
				"yarn",
			].includes(name)
		) {
			mutating = true;
			return true;
		}
		if (
			(name === "sed" || name === "gsed") &&
			args.some((arg) => arg === "-i" || arg.startsWith("-i"))
		) {
			mutating = true;
			return true;
		}
		if (name === "perl" && args.some((arg) => arg.includes("i"))) {
			mutating = true;
			return true;
		}
		if (
			name === "git" &&
			["clean", "reset", "checkout", "restore", "branch", "push"].includes(
				args[0] ?? "",
			)
		) {
			mutating = true;
			return true;
		}
	});
	return mutating;
}

function astCommandLooksDeleting(program: Program): boolean {
	let deleting = false;
	walkCommands(program, (cmd) => {
		const name = commandName(cmd);
		const args = commandWords(cmd).slice(1);
		if (!name) return;
		if (["rm", "rmdir", "unlink", "shred", "trash", "mv"].includes(name)) {
			deleting = true;
			return true;
		}
		if (name === "git" && args[0] === "clean") {
			deleting = true;
			return true;
		}
		if (
			name === "git" &&
			args[0] === "branch" &&
			args.some((arg) => /^-\S*D\b/.test(arg))
		) {
			deleting = true;
			return true;
		}
		if (name === "git" && args[0] === "push" && args.includes("--delete")) {
			deleting = true;
			return true;
		}
	});
	return deleting;
}

function appendNestedWordPathTokens(word: Word, tokens: string[]): void {
	for (const part of word.parts) {
		if (part.type === "DblQuoted") {
			appendNestedWordPathTokens({ type: "Word", parts: part.parts }, tokens);
		} else if (part.type === "ParamExp" && part.value) {
			appendNestedWordPathTokens(part.value, tokens);
		} else if (part.type === "CmdSubst" || part.type === "ProcSubst") {
			walkStatements(part.stmts, (nested) => {
				appendSimpleCommandPathTokens(nested, tokens);
				return undefined;
			});
		}
	}
}

function appendSimpleCommandPathTokens(
	cmd: SimpleCommand,
	tokens: string[],
): void {
	const words = cmd.words ?? [];
	if (words.length > 1) tokens.push(...words.slice(1).map(wordToString));
	for (const word of words) appendNestedWordPathTokens(word, tokens);
	for (const redirect of cmd.redirects ?? []) {
		tokens.push(wordToString(redirect.target));
		appendNestedWordPathTokens(redirect.target, tokens);
	}
}

function astPathTokens(program: Program): string[] {
	const tokens: string[] = [];
	walkCommands(program, (cmd) => {
		appendSimpleCommandPathTokens(cmd, tokens);
		return undefined;
	});
	return tokens;
}

function looksLikeWebPath(token: string): boolean {
	if (!token.startsWith("/")) return false;
	if (/[?#]/.test(token)) return true;
	if (/^\/(etc|usr|var|bin|sbin|lib|lib64|opt|home|root|proc|sys|dev|run|tmp|boot|srv|mnt|media|private)\b/.test(token))
		return false;
	if (/\/\.[^/]/.test(token)) return false;
	if (/\/[^/]*\.[^/]/.test(token)) return false;
	if (/\/[A-Z]/.test(token)) return false;
	return /^\/[a-z0-9_/%-]+$/.test(token);
}

function isWebContextCommand(command: string): boolean {
	return /\bfetch\s*\(|\bcurl\b|\bwget\b|\bhttpie\b|\bhttp\s+[A-Z]|\bagent-browser\b/i.test(
		command,
	);
}

function cleanCommandPathToken(word: string): string | null {
	const withoutRedirect = word.replace(/^[<>]+/, "").replace(/[,:;]+$/, "");
	if (
		!withoutRedirect ||
		withoutRedirect.startsWith("-") ||
		/^[A-Za-z_][A-Za-z0-9_]*=/.test(withoutRedirect)
	)
		return null;
	if (/^[a-z][a-z0-9+.-]+:\/\//i.test(withoutRedirect)) return null;
	if (
		/^\.\.?($|\/)/.test(withoutRedirect) ||
		withoutRedirect.startsWith("~/") ||
		withoutRedirect.startsWith("/") ||
		withoutRedirect.includes("/")
	)
		return withoutRedirect;
	return null;
}

export function outsideCwdPathsInCommand(
	command: string,
	cwd: string,
): string[] {
	const cmd = stripHeredocs(command);
	const cwdAbs = path.resolve(cwd);
	const cwdReal = fs.realpathSync.native(cwd);
	const paths = new Set<string>();
	const parsed = parseShell(command);
	const webContext = isWebContextCommand(cmd);
	const words = parsed ? astPathTokens(parsed) : splitCommandWords(cmd);

	for (const word of words) {
		const token = cleanCommandPathToken(word);
		if (!token) continue;
		if (webContext && looksLikeWebPath(token)) continue;
		const resolved = absoluteTarget(token, cwd);
		const normalized = fs.existsSync(resolved)
			? fs.realpathSync.native(resolved)
			: path.normalize(resolved);
		if (
			!isInsideOrSame(path.normalize(resolved), cwdAbs) &&
			!isInsideOrSame(normalized, cwdReal)
		)
			paths.add(normalized);
	}

	return [...paths];
}

export function evaluateBash(
	command: string,
	config: Config,
): { rule?: Rule; reason: string; ask: boolean } | null {
	const cmd = stripHeredocs(command);
	const parsed = parseShell(command);
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

	const zero = matchLiteralPathInCommand(cmd, config.zeroAccessPaths);
	if (zero)
		return {
			rule: zero,
			reason:
				zero.reason ?? `bash references zero-access path: ${ruleValue(zero)}`,
			ask: zero.ask === true,
		};

	if (
		(parsed ? astCommandLooksMutating(parsed) : commandLooksMutating(cmd)) ||
		/>{1,2}/.test(cmd)
	) {
		const readOnly = matchLiteralPathInCommand(cmd, config.readOnlyPaths);
		if (readOnly)
			return {
				rule: readOnly,
				reason:
					readOnly.reason ??
					`bash may modify read-only path: ${ruleValue(readOnly)}`,
				ask: readOnly.ask === true,
			};
	}

	if (parsed ? astCommandLooksDeleting(parsed) : commandLooksDeleting(cmd)) {
		const noDelete = matchLiteralPathInCommand(cmd, config.noDeletePaths);
		if (noDelete)
			return {
				rule: noDelete,
				reason:
					noDelete.reason ??
					`bash may delete protected path: ${ruleValue(noDelete)}`,
				ask: noDelete.ask === true,
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
