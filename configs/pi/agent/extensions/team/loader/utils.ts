import type { ParsedModel } from "./types";

export type AgentDiscoveryWarning = {
	filePath: string;
	message: string;
};

export const createDiscoveryWarning = (
	filePath: string,
	message: string,
): AgentDiscoveryWarning => ({
	filePath,
	message,
});

export type ModelDeclarationIssue = {
	value: unknown;
	reason: "invalid-type" | "invalid-selector";
};

export type ParsedModelDeclaration = {
	model?: string | string[];
	parsedModels: ParsedModel[];
	issues: ModelDeclarationIssue[];
};

const hasAlphanumericToken = (value: string): boolean => /[a-z0-9]/i.test(value);

/** Parses the provider/query syntax shared by frontmatter and team.yaml. */
export const parseModel = (value: unknown): ParsedModel | null => {
	if (typeof value !== "string") return null;
	const slashIndex = value.indexOf("/");
	if (slashIndex === -1) return null;

	const provider = value.slice(0, slashIndex).trim();
	const modelId = value.slice(slashIndex + 1).trim();
	if (!provider || !modelId || !hasAlphanumericToken(modelId)) return null;
	return { provider, modelId };
};

export const parseModelDeclaration = (value: unknown): ParsedModelDeclaration => {
	const values = typeof value === "string" ? [value] : Array.isArray(value) ? value : undefined;
	if (!values) {
		return {
			parsedModels: [],
			issues: [{ value, reason: "invalid-type" }],
		};
	}

	const parsedModels: ParsedModel[] = [];
	const declarations: string[] = [];
	const issues: ModelDeclarationIssue[] = [];
	for (const value of values) {
		if (typeof value !== "string") {
			issues.push({ value, reason: "invalid-type" });
			continue;
		}

		const declaration = value.trim();
		const parsed = parseModel(declaration);
		if (!parsed) {
			issues.push({ value, reason: "invalid-selector" });
			continue;
		}
		declarations.push(declaration);
		parsedModels.push(parsed);
	}

	return {
		model:
			declarations.length === 1
				? declarations[0]
				: declarations.length > 1
					? declarations
					: undefined,
		parsedModels,
		issues,
	};
};
