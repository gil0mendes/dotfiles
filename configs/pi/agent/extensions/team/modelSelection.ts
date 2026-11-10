import type { ParsedModel } from "./loader/types";

export type AvailableModelCandidate = { provider: string; id: string; name?: string };
export type AvailableModelSelection<T> =
	| { kind: "matched"; model: T }
	| { kind: "unavailable" }
	| { kind: "ambiguous"; candidates: readonly T[] };

const fold = (value: string): string => value.trim().toLowerCase();
const normalize = (value: string): string =>
	fold(value)
		.replace(/[.\-_\s]+/g, " ")
		.trim();
const tokens = (value: string): string[] => value.split(" ").filter(Boolean);
const legacyTokens = (value: string): string[] =>
	value
		.toLowerCase()
		.split(/[^a-z0-9]+/i)
		.filter(Boolean);

const fromMatches = <T>(matches: readonly T[]): AvailableModelSelection<T> => {
	if (matches.length === 0) return { kind: "unavailable" };
	const model = matches[0];
	if (matches.length === 1 && model !== undefined) return { kind: "matched", model };
	return { kind: "ambiguous", candidates: matches };
};

const bareScore = (query: string, candidate: AvailableModelCandidate): number => {
	const raw = fold(query);
	if (!raw) return 0;
	const id = fold(candidate.id);
	const name = fold(candidate.name ?? "");
	if (raw === id) return 100;
	if (name && raw === name) return 90;
	const normalized = normalize(raw);
	if (!normalized) return 0;
	const normalizedId = normalize(id);
	const normalizedName = normalize(name);
	if (normalized === normalizedId) return 80;
	if (normalizedName && normalized === normalizedName) return 75;
	const queryTokens = tokens(normalized);
	const idTokens = tokens(normalizedId);
	if (
		queryTokens.length <= idTokens.length &&
		queryTokens.every((token, i) => idTokens[i]?.startsWith(token))
	)
		return 70;
	if (normalizedId.includes(normalized)) return 60;
	if (normalizedName && normalizedName.includes(normalized)) return 40;
	if (queryTokens.every((token) => normalizedId.includes(token) || normalizedName.includes(token)))
		return 20;
	return 0;
};

const matchesLegacyTokens = (query: readonly string[], id: string): boolean => {
	if (query.length === 0) return false;
	const candidate = legacyTokens(id);
	let index = 0;
	for (const token of query) {
		while (index < candidate.length && !candidate[index]?.startsWith(token)) index++;
		if (index === candidate.length) return false;
		index++;
	}
	return true;
};

export const selectAvailableModel = <T extends AvailableModelCandidate>(
	selector: ParsedModel,
	availableModels: readonly T[],
): AvailableModelSelection<T> => {
	const seen = new Set<string>();
	const models = availableModels.filter((model) => {
		const identity = JSON.stringify([model.provider, model.id]);
		if (seen.has(identity)) return false;
		seen.add(identity);
		return true;
	});
	if (selector.kind === "qualified") {
		const literal = models.find(
			(model) => model.provider === selector.provider && model.id === selector.modelId,
		);
		if (literal) return { kind: "matched", model: literal };
		const providerModels = models.filter(
			(model) => fold(model.provider) === fold(selector.provider),
		);
		const exact = providerModels.filter((model) => fold(model.id) === fold(selector.modelId));
		if (exact.length) return fromMatches(exact);
		const queryTokens = legacyTokens(selector.modelId);
		return fromMatches(
			providerModels.filter((model) => matchesLegacyTokens(queryTokens, model.id)),
		);
	}
	let highest = 0;
	let matches: T[] = [];
	for (const model of models) {
		const score = bareScore(selector.query, model);
		if (score > highest) {
			highest = score;
			matches = [model];
		} else if (score > 0 && score === highest) matches.push(model);
	}
	return fromMatches(matches);
};
