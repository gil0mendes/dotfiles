import * as fs from "node:fs";
import * as path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import type { RawConfig } from "./config";

const { loadYamlConfig, parseSimpleYaml } = await import("./config");

type TestConfig = {
	enabled: boolean;
	name: string;
	items: string[];
	rules: Array<{ pattern: string; ask: boolean }>;
};

const defaultConfig: TestConfig = {
	enabled: false,
	name: "default",
	items: [],
	rules: [],
};

const tempDirs: string[] = [];
let tempIndex = 0;

function makeTempDir(): string {
	const dir = path.join("/tmp", `pi-config-test-${tempIndex++}`);
	fs.mkdirSync(dir, { recursive: true });
	tempDirs.push(dir);
	return dir;
}

function stringValue(raw: RawConfig, key: string, fallback: string): string {
	return typeof raw[key] === "string" ? raw[key] : fallback;
}

function booleanValue(raw: RawConfig, key: string, fallback: boolean): boolean {
	return typeof raw[key] === "boolean" ? raw[key] : fallback;
}

function stringList(raw: RawConfig, key: string): string[] {
	return Array.isArray(raw[key])
		? raw[key].filter((item): item is string => typeof item === "string")
		: [];
}

function parseTestConfig(raw: RawConfig): TestConfig {
	return {
		enabled: booleanValue(raw, "enabled", defaultConfig.enabled),
		name: stringValue(raw, "name", defaultConfig.name),
		items: stringList(raw, "items"),
		rules: Array.isArray(raw.rules)
			? raw.rules.flatMap((item) => {
					if (!item || typeof item !== "object") return [];
					const rule = item as Record<string, unknown>;
					return typeof rule.pattern === "string"
						? [{ pattern: rule.pattern, ask: rule.ask === true }]
						: [];
				})
			: [],
	};
}

afterEach(() => {
	for (const dir of tempDirs.splice(0)) {
		fs.rmSync(dir, { recursive: true, force: true });
	}
});

describe("parseSimpleYaml", () => {
	it("parses scalars, string lists, object lists, booleans, and comments", () => {
		const raw = parseSimpleYaml(`
# leading comment
enabled: true
name: 'custom # not comment'
items:
  - one
  - "two"
rules:
  - pattern: danger # trailing comment
    ask: true
  - pattern: safe
    ask: false
`);

		expect(raw).toEqual({
			enabled: true,
			name: "custom # not comment",
			items: ["one", "two"],
			rules: [
				{ pattern: "danger", ask: true },
				{ pattern: "safe", ask: false },
			],
		});
	});
});

describe("loadYamlConfig", () => {
	it("loads project config before global config and returns the typed parser result", () => {
		const cwd = makeTempDir();
		const globalDir = makeTempDir();
		const projectConfig = path.join(cwd, ".pi", "tool.yaml");
		const globalConfig = path.join(globalDir, "tool.yaml");
		fs.mkdirSync(path.dirname(projectConfig), { recursive: true });
		fs.writeFileSync(globalConfig, "name: global\n");
		fs.writeFileSync(
			projectConfig,
			"enabled: true\nname: project\nitems:\n  - alpha\n",
		);

		const result = loadYamlConfig<TestConfig>({
			cwd,
			projectRelativePath: path.join(".pi", "tool.yaml"),
			configPaths: [globalConfig],
			defaultConfig,
			parse: parseTestConfig,
		});

		expect(result).toEqual({
			source: projectConfig,
			config: {
				enabled: true,
				name: "project",
				items: ["alpha"],
				rules: [],
			},
		});
	});

	it("returns the default config when no config file exists", () => {
		const result = loadYamlConfig<TestConfig>({
			cwd: makeTempDir(),
			projectRelativePath: path.join(".pi", "tool.yaml"),
			configPaths: [path.join(makeTempDir(), "missing.yaml")],
			defaultConfig,
			parse: parseTestConfig,
		});

		expect(result).toEqual({ config: defaultConfig, source: null });
	});

	it("returns the default config and error when typed parsing fails", () => {
		const cwd = makeTempDir();
		const configPath = path.join(cwd, "tool.yaml");
		fs.writeFileSync(configPath, "enabled: true\n");

		const result = loadYamlConfig<TestConfig>({
			cwd,
			configPaths: [configPath],
			defaultConfig,
			parse: () => {
				throw new Error("invalid tool config");
			},
		});

		expect(result.config).toBe(defaultConfig);
		expect(result.source).toBe(configPath);
		expect(result.error).toBe("invalid tool config");
	});
});
