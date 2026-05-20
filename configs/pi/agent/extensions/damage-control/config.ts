import * as os from "node:os";
import * as path from "node:path";
import { loadYamlConfig, type RawConfig } from "../_shared/config";
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

function normalizeConfig(loaded: RawConfig): Config {
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

export const configs = (() => {
	let source: string | null = null;
	let current: Config = DEFAULT_CONFIG;

	return {
		get current() {
			return current;
		},
		get source() {
			return source;
		},
		load: (cwd: string) => {
			const result = loadYamlConfig({
				cwd,
				projectRelativePath: path.join(".pi", "damage-control-rules.yaml"),
				configPaths: CONFIG_PATHS,
				defaultConfig: DEFAULT_CONFIG,
				parse: normalizeConfig,
			});
			source = result.source;
			current = result.config;
			return result;
		},
		totalRules: () =>
			(current.useBuiltinCommandMatchers
				? BUILTIN_DANGEROUS_COMMAND_COUNT
				: 0) +
			current.bashToolPatterns.length +
			current.zeroAccessPaths.length +
			current.readOnlyPaths.length +
			current.noDeletePaths.length +
			current.strictModeAllowedList.length,
	};
})();
