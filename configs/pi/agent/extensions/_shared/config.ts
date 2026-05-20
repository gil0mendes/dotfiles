import * as fs from "node:fs";
import * as path from "node:path";

export type RawConfig = Record<string, unknown>;

export type ConfigLoaderOptions<TConfig> = {
	cwd: string;
	projectRelativePath?: string;
	configPaths: readonly string[];
	defaultConfig: TConfig;
	parse: (raw: RawConfig) => TConfig;
};

export type ConfigLoadResult<TConfig> = {
	config: TConfig;
	source: string | null;
	error?: string;
};

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

export function parseSimpleYaml(input: string): RawConfig {
	const out: RawConfig = {};
	let section: string | null = null;
	let currentItem: Record<string, unknown> | null = null;

	for (const rawLine of input.split(/\r?\n/)) {
		const line = removeComment(rawLine).replace(/\t/g, "  ").trimEnd();
		if (!line.trim()) continue;

		const top = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
		if (top && top[1] && top[2] !== undefined && !rawLine.startsWith(" ")) {
			section = top[1];
			const rest = top[2].trim();
			out[section] = rest === "" ? [] : parseScalar(rest);
			currentItem = null;
			continue;
		}

		if (!section) continue;
		const arr = Array.isArray(out[section])
			? (out[section] as unknown[])
			: (out[section] = [] as unknown[]);
		const item = line.match(/^\s*-\s*(.*)$/);
		if (item && item[1] !== undefined) {
			const rest = item[1].trim();
			const kv = rest.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
			if (kv && kv[1] && kv[2] !== undefined) {
				currentItem = { [kv[1]]: parseScalar(kv[2]) };
				arr.push(currentItem);
			} else {
				arr.push(parseScalar(rest));
				currentItem = null;
			}
			continue;
		}

		const nested = line.match(/^\s+([A-Za-z0-9_-]+):\s*(.*)$/);
		if (nested && nested[1] && nested[2] !== undefined && currentItem)
			currentItem[nested[1]] = parseScalar(nested[2]);
	}

	return out;
}

export function findConfigPath(
	cwd: string,
	projectRelativePath: string | undefined,
	configPaths: readonly string[],
): string | null {
	if (projectRelativePath) {
		const projectPath = path.join(cwd, projectRelativePath);
		if (fs.existsSync(projectPath)) return projectPath;
	}
	return configPaths.find((candidate) => fs.existsSync(candidate)) ?? null;
}

export function loadYamlConfig<TConfig>({
	cwd,
	projectRelativePath,
	configPaths,
	defaultConfig,
	parse,
}: ConfigLoaderOptions<TConfig>): ConfigLoadResult<TConfig> {
	const source = findConfigPath(cwd, projectRelativePath, configPaths);
	if (!source) return { config: defaultConfig, source: null };

	try {
		return {
			config: parse(parseSimpleYaml(fs.readFileSync(source, "utf8"))),
			source,
		};
	} catch (error) {
		return {
			config: defaultConfig,
			source,
			error: error instanceof Error ? error.message : String(error),
		};
	}
}
