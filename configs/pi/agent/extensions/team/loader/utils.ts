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

export const parseModel = (value: unknown): ParsedModel | null => {
	if (typeof value !== "string" || !value.includes("/")) return null;
	const slashIndex = value.indexOf("/");
	const provider = value.slice(0, slashIndex).trim();
	const modelId = value.slice(slashIndex + 1).trim();
	return provider && modelId ? { provider, modelId } : null;
};

export const parseModelPreferences = (value: unknown): string[] | null => {
	if (typeof value === "string") {
		const model = value.trim();
		return model ? [model] : [];
	}

	if (!Array.isArray(value)) {
		return null;
	}

	const models: string[] = [];
	for (const item of value) {
		if (typeof item !== "string") {
			return null;
		}
		const model = item.trim();
		if (model) {
			models.push(model);
		}
	}
	return models;
};
