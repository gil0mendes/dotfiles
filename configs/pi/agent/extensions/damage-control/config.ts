import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { BUILTIN_DANGEROUS_COMMAND_COUNT } from "./commands/dangerous";
import type { Config, Rule } from "./types";

export const DEFAULT_CONFIG: Config = {
	strictMode: false,
	useBuiltinCommandMatchers: true,
	askBeforeOutsideCwd: false,
	allowedOutsideCwdPaths: [],
	bashToolPatterns: [],
	zeroAccessPaths: [],
	readOnlyPaths: [],
	noDeletePaths: [],
	strictModeAllowedList: [],
};

export const DEFAULT_CONFIG_PATH = path.join(
	os.homedir(),
	".pi",
	"agent",
	"settings",
	"damage-control-rules.yaml",
);

const CONFIG_PATHS = [
	DEFAULT_CONFIG_PATH,
	path.join(os.homedir(), ".pi", "damage-control-rules.yaml"),
];

function stripQuotes(value: string): string {
	const trimmed = value.trim();
	if (
		(trimmed.startsWith('"') && trimmed.endsWith('"')) ||
		(trimmed.startsWith("'") && trimmed.endsWith("'"))
	) {
		return trimmed.slice(1, -1);
	}
	return trimmed;
}

function parseScalar(value: string): string | boolean {
	const unquoted = stripQuotes(value);
	if (unquoted === "true") return true;
	if (unquoted === "false") return false;
	return unquoted;
}

function removeComment(line: string): string {
	let quote: string | null = null;
	for (let i = 0; i < line.length; i++) {
		const ch = line[i] ?? "";
		if ((ch === '"' || ch === "'") && line[i - 1] !== "\\")
			quote = quote === ch ? null : (quote ?? ch);
		if (ch === "#" && !quote && (i === 0 || /\s/.test(line[i - 1] ?? "")))
			return line.slice(0, i);
	}
	return line;
}

function parseSimpleYaml(input: string): Partial<Config> {
	const out: Record<string, unknown> = {};
	let section: string | null = null;
	let currentItem: Record<string, unknown> | null = null;

	for (const rawLine of input.split(/\r?\n/)) {
		const line = removeComment(rawLine).replace(/\t/g, "  ").trimEnd();
		if (!line.trim()) continue;

		const top = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
		if (top && top[1] && top[2] !== undefined && !rawLine.startsWith(" ")) {
			section = top[1];
			const rest = top[2].trim();
			if (rest === "") out[section] = [];
			else out[section] = parseScalar(rest);
			currentItem = null;
			continue;
		}

		if (!section) continue;
		const sectionName = section;
		const arr = Array.isArray(out[sectionName])
			? (out[sectionName] as unknown[])
			: (out[sectionName] = [] as unknown[]);
		const item = line.match(/^\s*-\s*(.*)$/);
		if (item && item[1] !== undefined) {
			const rest = item[1].trim();
			const kv = rest.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
			if (kv && kv[1] && kv[2] !== undefined) {
				currentItem = { [kv[1]]: parseScalar(kv[2]) };
				arr.push(currentItem);
			} else if (section === "allowedOutsideCwdPaths") {
				arr.push(parseScalar(rest));
				currentItem = null;
			} else {
				currentItem = { pattern: parseScalar(rest) };
				arr.push(currentItem);
			}
			continue;
		}

		const nested = line.match(/^\s+([A-Za-z0-9_-]+):\s*(.*)$/);
		if (nested && nested[1] && nested[2] !== undefined && currentItem)
			currentItem[nested[1]] = parseScalar(nested[2]);
	}

	return out as Partial<Config>;
}

function normalizeRule(
	raw: unknown,
	defaultKey: "pattern" | "path" | "command",
): Rule | null {
	if (typeof raw === "string")
		return { [defaultKey]: raw, reason: `${defaultKey} ${raw} is protected` };
	if (!raw || typeof raw !== "object") return null;
	const rule = raw as Record<string, unknown>;
	const value = rule.pattern ?? rule.path ?? rule.command;
	if (typeof value !== "string") return null;
	return {
		pattern: typeof rule.pattern === "string" ? rule.pattern : undefined,
		path: typeof rule.path === "string" ? rule.path : undefined,
		command: typeof rule.command === "string" ? rule.command : undefined,
		reason:
			typeof rule.reason === "string"
				? rule.reason
				: `${defaultKey} ${value} is protected`,
		ask: rule.ask === true,
	};
}

function normalizeConfig(loaded: Partial<Config>): Config {
	const normalize = (
		key: keyof Config,
		defaultKey: "pattern" | "path" | "command",
	) =>
		(Array.isArray(loaded[key]) ? (loaded[key] as unknown[]) : [])
			.map((r) => normalizeRule(r, defaultKey))
			.filter((r): r is Rule => Boolean(r));

	const normalizeStrings = (key: keyof Config) =>
		(Array.isArray(loaded[key]) ? (loaded[key] as unknown[]) : []).filter(
			(v): v is string => typeof v === "string",
		);

	return {
		strictMode: loaded.strictMode === true,
		useBuiltinCommandMatchers: loaded.useBuiltinCommandMatchers !== false,
		askBeforeOutsideCwd: loaded.askBeforeOutsideCwd === true,
		allowedOutsideCwdPaths: normalizeStrings("allowedOutsideCwdPaths"),
		bashToolPatterns: normalize("bashToolPatterns", "pattern"),
		zeroAccessPaths: normalize("zeroAccessPaths", "path"),
		readOnlyPaths: normalize("readOnlyPaths", "path"),
		noDeletePaths: normalize("noDeletePaths", "path"),
		strictModeAllowedList: normalize("strictModeAllowedList", "command"),
	};
}

function findConfigPath(cwd: string): string | null {
	const projectPath = path.join(cwd, ".pi", "damage-control-rules.yaml");
	if (fs.existsSync(projectPath)) return projectPath;
	return CONFIG_PATHS.find((p) => fs.existsSync(p)) ?? null;
}

export function loadConfig(cwd: string): {
	config: Config;
	source: string | null;
	error?: string;
} {
	const source = findConfigPath(cwd);
	if (!source) return { config: DEFAULT_CONFIG, source: null };
	try {
		return {
			config: normalizeConfig(parseSimpleYaml(fs.readFileSync(source, "utf8"))),
			source,
		};
	} catch (error) {
		return {
			config: DEFAULT_CONFIG,
			source,
			error: error instanceof Error ? error.message : String(error),
		};
	}
}

export function totalRules(config: Config): number {
	return (
		(config.useBuiltinCommandMatchers ? BUILTIN_DANGEROUS_COMMAND_COUNT : 0) +
		config.bashToolPatterns.length +
		config.zeroAccessPaths.length +
		config.readOnlyPaths.length +
		config.noDeletePaths.length +
		config.strictModeAllowedList.length
	);
}
