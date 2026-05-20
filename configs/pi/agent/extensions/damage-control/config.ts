import * as os from "node:os";
import * as path from "node:path";
import { loadYamlConfig, type RawConfig } from "../_shared/config";
import { BUILTIN_DANGEROUS_COMMAND_COUNT } from "./commands/dangerous";
import { DEFAULT_FILESYSTEM_CONFIG, mergeFilesystemConfig } from "./filesystem";
import type {
	BashToolPatternConfig,
	Config,
	FilesystemGateConfig,
	StrictModeAllowedCommandConfig,
} from "./types";

export const DEFAULT_CONFIG: Config = {
	permissionGate: true,
	strictMode: false,
	useBuiltinCommandMatchers: true,
	filesystem: DEFAULT_FILESYSTEM_CONFIG,
	bashToolPatterns: [],
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

function isRawObject(raw: unknown): raw is Record<string, unknown> {
	return raw !== null && typeof raw === "object";
}

function configReason(
	rule: Record<string, unknown>,
	defaultKey: "pattern" | "command",
	value: string,
): string {
	return typeof rule.reason === "string"
		? rule.reason
		: `${defaultKey} ${value} is protected`;
}

function normalizeBashToolPattern(raw: unknown): BashToolPatternConfig | null {
	if (typeof raw === "string")
		return { pattern: raw, reason: `pattern ${raw} is protected` };
	if (!isRawObject(raw) || typeof raw.pattern !== "string") return null;
	return {
		pattern: raw.pattern,
		reason: configReason(raw, "pattern", raw.pattern),
		ask: raw.ask === true,
	};
}

function normalizeStrictModeCommand(
	raw: unknown,
): StrictModeAllowedCommandConfig | null {
	if (typeof raw === "string")
		return { command: raw, reason: `command ${raw} is protected` };
	if (!isRawObject(raw) || typeof raw.command !== "string") return null;
	return {
		command: raw.command,
		reason: configReason(raw, "command", raw.command),
	};
}

function normalizeStringArray(raw: unknown): string[] | undefined {
	return Array.isArray(raw)
		? raw.filter((value): value is string => typeof value === "string")
		: undefined;
}

function normalizeFilesystem(raw: unknown): FilesystemGateConfig {
	if (!isRawObject(raw)) return DEFAULT_FILESYSTEM_CONFIG;
	return mergeFilesystemConfig(DEFAULT_FILESYSTEM_CONFIG, {
		denyRead: normalizeStringArray(raw.denyRead),
		allowRead: normalizeStringArray(raw.allowRead),
		allowWrite: normalizeStringArray(raw.allowWrite),
		denyWrite: normalizeStringArray(raw.denyWrite),
		allowGitConfig:
			typeof raw.allowGitConfig === "boolean" ? raw.allowGitConfig : undefined,
	});
}

function normalizeConfig(loaded: RawConfig): Config {
	const normalize = <T>(
		key: keyof Config,
		parse: (raw: unknown) => T | null,
	): T[] =>
		(Array.isArray(loaded[key]) ? (loaded[key] as unknown[]) : [])
			.map((r) => parse(r))
			.filter((r): r is T => Boolean(r));

	return {
		permissionGate: loaded.permissionGate !== false,
		strictMode: loaded.strictMode === true,
		useBuiltinCommandMatchers: loaded.useBuiltinCommandMatchers !== false,
		filesystem: normalizeFilesystem(loaded.filesystem),
		bashToolPatterns: normalize(
			"bashToolPatterns",
			normalizeBashToolPattern,
		),
		strictModeAllowedList: normalize(
			"strictModeAllowedList",
			normalizeStrictModeCommand,
		),
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
			(current.permissionGate === false
				? 0
				: (current.useBuiltinCommandMatchers
						? BUILTIN_DANGEROUS_COMMAND_COUNT
						: 0) +
					current.bashToolPatterns.length +
					current.strictModeAllowedList.length) +
			current.filesystem.denyRead.length +
			(current.filesystem.allowRead?.length ?? 0) +
			current.filesystem.allowWrite.length +
			current.filesystem.denyWrite.length,
	};
})();
