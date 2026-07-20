import * as path from "node:path";
import * as fs from "node:fs";

import { isNil } from "ramda";
import { getAgentDir, parseFrontmatter } from "@earendil-works/pi-coding-agent";
import {
	parseModel,
	parseModelPreferences,
	type AgentDiscoveryWarning,
} from "./utils";
import type {
	AgentConfig,
	AgentConfigFields,
	AgentType,
	ParsedModel,
} from "./types";
import type { ThinkingLevel } from "@earendil-works/pi-ai";

export type AgentDefinitionFile = {
	filePath: string;
	content: string | null;
	warnings?: AgentDiscoveryWarning[];
};

export type AgentDefinitionSourceGroup = {
	agentsDir: string;
	files: AgentDefinitionFile[];
	warnings?: AgentDiscoveryWarning[];
};

export type AgentCatalogSource = {
	loadAgentDefinitionGroups(cwd: string): AgentDefinitionSourceGroup[];
	loadConfigFiles(cwd: string): AgentDefinitionFile[];
};

type AgentConfigFile = {
	filePath: string;
	content: string | null;
	warnings?: AgentDiscoveryWarning[];
};

const createDiscoveryWarning = (
	filePath: string,
	message: string,
): AgentDiscoveryWarning => ({
	filePath,
	message,
});

const loadAgentFile = (filePath: string): AgentDefinitionFile => {
	try {
		return { filePath, content: fs.readFileSync(filePath, "utf-8") };
	} catch (error) {
		const reason = error instanceof Error ? error.message : String(error);
		return {
			filePath,
			content: null,
			warnings: [
				createDiscoveryWarning(
					filePath,
					`Failed to read agent file: ${reason}`,
				),
			],
		};
	}
};

const loadAgentDefinitionGroup = (
	agentsDir: string,
): AgentDefinitionSourceGroup | null => {
	const dirToLoad = path.resolve(agentsDir);
	if (!fs.existsSync(dirToLoad)) {
		return null;
	}

	let entries: fs.Dirent[];
	try {
		entries = fs.readdirSync(dirToLoad, { withFileTypes: true });
	} catch (error) {
		const reason = error instanceof Error ? error.message : String(error);
		return {
			agentsDir,
			files: [],
			warnings: [
				createDiscoveryWarning(
					agentsDir,
					`Failed to read agent directory: ${reason}`,
				),
			],
		};
	}

	return {
		agentsDir,
		files: entries
			.filter((entry) => entry.name.endsWith(".md"))
			.filter((entry) => entry.isFile() || entry.isSymbolicLink())
			.map((entry) => loadAgentFile(path.join(dirToLoad, entry.name))),
	};
};

const loadConfigFile = (filePath: string): AgentConfigFile | null => {
	const fileToLoad = path.resolve(filePath);
	if (!fs.existsSync(fileToLoad)) {
		return null;
	}

	try {
		const content = fs.readFileSync(fileToLoad, "utf8");
		return { filePath: fileToLoad, content };
	} catch (error) {
		const reason = error instanceof Error ? error.message : String(error);
		return {
			filePath: fileToLoad,
			content: null,
			warnings: [
				createDiscoveryWarning(
					fileToLoad,
					`Failed to read config file: ${reason}`,
				),
			],
		};
	}
};

type AgentConfigOverride = AgentConfigFields;
type ParsedFieldName =
	"model" | "thinking" | "tools" | "skills" | "compaction" | "interactive";
type ParsedListFieldName = "tools" | "skills";
type ParsedBooleanFieldName = "compaction" | "interactive";
type WarningSubject = "subagent" | "subagent override";

type ParsedFieldWarning =
	| { code: "invalid-list-format"; fieldName: ParsedListFieldName }
	| {
			code: "invalid-type";
			fieldName: ParsedFieldName;
			expected: "string" | "string or YAML string array" | "boolean";
	  }
	| { code: "invalid-model-format"; model: string }
	| { code: "invalid-thinking-level"; thinking: string };

type ParsedFieldSet = AgentConfigFields & {
	warnings: ParsedFieldWarning[];
};

type ParseFieldOptions = {
	warnOnInvalidType: boolean;
	setValueOnInvalidType: boolean;
};

const parseAgentType = (value: unknown): AgentType | null => {
	if (value === undefined) {
		return "agent";
	}
	if (value === "agent" || value === "orchestrator") {
		return value;
	}
	return null;
};

const VALID_THINKING_LEVELS: readonly string[] = [
	"off",
	"minimal",
	"low",
	"medium",
	"high",
	"xhigh",
];

const validateThinkingLevel = (
	value: string | undefined,
): ThinkingLevel | undefined => {
	if (!value) {
		return undefined;
	}

	return VALID_THINKING_LEVELS.includes(value)
		? (value as ThinkingLevel)
		: undefined;
};

const parseCommaSeparated = (value: unknown): string[] | undefined => {
	if (isNil(value)) {
		return undefined;
	}
	if (Array.isArray(value))
		return value.map((v) => String(v).trim()).filter(Boolean);
	if (typeof value === "string")
		return value
			.split(",")
			.map((s) => s.trim())
			.filter(Boolean);
	return undefined;
};

const parseListField = (
	value: unknown,
	fieldName: ParsedListFieldName,
): { values: string[]; warnings: ParsedFieldWarning[] } => {
	if (isNil(value)) {
		return { values: [], warnings: [] };
	}

	const parsed = parseCommaSeparated(value);
	if (!isNil(parsed)) {
		return { values: parsed, warnings: [] };
	}

	return { values: [], warnings: [{ code: "invalid-list-format", fieldName }] };
};

const parseBooleanField = (
	fieldName: ParsedBooleanFieldName,
	value: unknown,
	options: ParseFieldOptions,
): Pick<ParsedFieldSet, ParsedBooleanFieldName | "warnings"> => {
	if (typeof value === "boolean") return { [fieldName]: value, warnings: [] };
	if (value !== undefined && options.warnOnInvalidType) {
		return {
			warnings: [{ code: "invalid-type", fieldName, expected: "boolean" }],
		};
	}
	return { warnings: [] };
};

const parseSharedFields = (
	record: Record<string, unknown>,
	options: ParseFieldOptions,
): ParsedFieldSet => {
	const warnings: ParsedFieldWarning[] = [];
	const fields: AgentConfigFields = {};

	if (!isNil(record.model)) {
		const modelPreferences = parseModelPreferences(record.model);
		if (modelPreferences) {
			if (modelPreferences.length === 1) {
				fields.model = modelPreferences[0];
			} else if (modelPreferences.length > 1) {
				fields.model = modelPreferences;
			}

			const parsedModels: ParsedModel[] = [];
			for (const model of modelPreferences) {
				const parsedModel = parseModel(model);
				if (parsedModel) {
					parsedModels.push(parsedModel);
				} else {
					warnings.push({ code: "invalid-model-format", model });
				}
			}

			fields.parsedModels = parsedModels;
			fields.parsedModel = parsedModels[0];
		} else if (options.warnOnInvalidType) {
			warnings.push({
				code: "invalid-type",
				fieldName: "model",
				expected: "string or YAML string array",
			});
		}
	}

	if (typeof record.thinking === "string") {
		const thinking = validateThinkingLevel(record.thinking);
		if (thinking) fields.thinking = thinking;
		else
			warnings.push({
				code: "invalid-thinking-level",
				thinking: record.thinking,
			});
	}

	if ("tools" in record) {
		const parsed = parseListField(record.tools, "tools");
		fields.tools = parsed.values;
		warnings.push(...parsed.warnings);
	}

	if ("skills" in record) {
		const parsed = parseListField(record.skills, "skills");
		fields.skills = parsed.values;
		warnings.push(...parsed.warnings);
	}

	if (typeof record.compaction === "boolean") {
		const parsed = parseBooleanField("compaction", record.compaction, options);
		fields.compaction = parsed.compaction;
		warnings.push(...parsed.warnings);
	}

	if (typeof record.interactive === "boolean") {
		const parsed = parseBooleanField(
			"interactive",
			record.interactive,
			options,
		);
		fields.interactive = parsed.interactive;
		warnings.push(...parsed.warnings);
	}

	return {
		...fields,
		warnings,
	};
};

type ParseResult = {
	agent: AgentConfig | null;
	warnings: AgentDiscoveryWarning[];
};

const formatFieldWarning = (
	subject: WarningSubject,
	name: string,
	warning: ParsedFieldWarning,
): string => {
	const prefix = `${subject === "subagent" ? "Subagent" : "Subagent override"} "${name}"`;

	switch (warning.code) {
		case "invalid-list-format":
			return `${prefix}: invalid ${warning.fieldName} field, expected a comma-separated string or YAML array`;
		case "invalid-type":
			return `${prefix}: field "${warning.fieldName}" must be a ${warning.expected}, ignoring`;
		case "invalid-model-format":
			return `${prefix}: invalid model format "${warning.model}" (expected "provider/model-id"), ignoring model`;
		case "invalid-thinking-level":
			return `${prefix}: invalid thinking level "${warning.thinking}", ignoring`;
	}
};

const toParseWarnings = (
	filePath: string,
	subject: WarningSubject,
	name: string,
	warnings: ParsedFieldWarning[],
): AgentDiscoveryWarning[] =>
	warnings.map((warning) =>
		createDiscoveryWarning(
			filePath,
			formatFieldWarning(subject, name, warning),
		),
	);

const parseDefinitionFields = (
	record: Record<string, unknown>,
	filePath: string,
	agentName: string,
): { fields: AgentConfigFields; warnings: AgentDiscoveryWarning[] } => {
	const parsed = parseSharedFields(record, {
		warnOnInvalidType: false,
		setValueOnInvalidType: true,
	});
	const { warnings, ...fields } = parsed;
	return {
		fields,
		warnings: toParseWarnings(filePath, "subagent", agentName, warnings),
	};
};

const parseAgentDefinition = (
	content: string,
	filePath: string,
): ParseResult => {
	let frontmatter: Record<string, unknown>;
	let body: string;
	try {
		const parsed = parseFrontmatter<Record<string, unknown>>(content);
		frontmatter = parsed.frontmatter;
		body = parsed.body;
	} catch (error) {
		const reason = error instanceof Error ? error.message : String(error);
		return {
			agent: null,
			warnings: [
				createDiscoveryWarning(
					filePath,
					`Ignored invalid subagent definition. Frontmatter could not be parsed: ${reason}`,
				),
			],
		};
	}

	const name =
		typeof frontmatter.name === "string" ? frontmatter.name.trim() : undefined;
	const description =
		typeof frontmatter.description === "string"
			? frontmatter.description.trim()
			: undefined;
	if (!name || !description) {
		return {
			agent: null,
			warnings: [
				createDiscoveryWarning(
					filePath,
					'Ignored invalid subagent definition. Required frontmatter fields "name" and "description" must be non-empty strings.',
				),
			],
		};
	}
	if (/\s/.test(name)) {
		return {
			agent: null,
			warnings: [
				createDiscoveryWarning(
					filePath,
					`Ignored subagent definition "${name}". Subagent names cannot contain whitespace. Use "-" instead.`,
				),
			],
		};
	}

	const type = parseAgentType(frontmatter.type);
	if (!type) {
		return {
			agent: null,
			warnings: [
				createDiscoveryWarning(
					filePath,
					`Ignored subagent definition "${name}". Frontmatter field "type" must be either "agent" or "orchestrator".`,
				),
			],
		};
	}

	const parsedFields = parseDefinitionFields(frontmatter, filePath, name);
	return {
		agent: {
			name,
			description,
			...parsedFields.fields,
			type,
			systemPrompt: body,
			filePath,
		},
		warnings: parsedFields.warnings,
	};
};

export const loadAgentDefinitionFromFile = (
	file: AgentDefinitionFile,
): ParseResult => {
	if (!file.content) return { agent: null, warnings: file.warnings ?? [] };
	const parsed = parseAgentDefinition(file.content, file.filePath);
	return {
		agent: parsed.agent,
		warnings: [...(file.warnings ?? []), ...parsed.warnings],
	};
};

export class FileSystemAgentCatalogSource implements AgentCatalogSource {
	loadAgentDefinitionGroups(cwd: string): AgentDefinitionSourceGroup[] {
		return [path.join(getAgentDir(), "agents"), path.join(cwd, ".pi", "agents")]
			.map(loadAgentDefinitionGroup)
			.filter((group): group is AgentDefinitionSourceGroup => group !== null);
	}

	loadConfigFiles(cwd: string): AgentDefinitionFile[] {
		return [
			path.join(getAgentDir(), "settings", "team.yaml"),
			path.join(cwd, "settings", "team.yaml"),
		]
			.map(loadConfigFile)
			.filter((file): file is AgentConfigFile => file !== null);
	}
}
