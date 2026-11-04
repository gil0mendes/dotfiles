import type { Api, AssistantMessage, Model } from "@earendil-works/pi-ai";
import type { SettledSubagentStatus, SubagentState, SubagentStatus } from "./types";
import {
	createAgentSession,
	DefaultResourceLoader,
	SessionManager,
	SettingsManager,
	type AgentSession,
	type ModelRegistry,
} from "@earendil-works/pi-coding-agent";
import type { AgentConfig, ParsedModel } from "./loader/types";
import type { AgentMessage } from "@earendil-works/pi-agent-core";

type StartOptions = {
	cwd: string;
	ctx: BootstrapContext;
	onWarning?: (message: string) => void;
};

export interface SubagentRunner {
	start(state: SubagentState, opts: StartOptions): Promise<void>;
	respond(state: SubagentState, message: string): void;
	abort(state: SubagentState): void;
}

export interface SubagentRunnerCallbacks {
	isCurrent: (state: SubagentState) => boolean;
	onProgress: (ownerSessionId: string) => void;
	onSessionAttached: (state: SubagentState, sessionId: string) => void;
	onSettled: (
		state: SubagentState,
		status: SettledSubagentStatus,
		outcome: { result?: string; error?: string },
	) => void;
}

export type BootstrapContext = {
	model?: Model<Api>;
	modelRegistry: ModelRegistry;
	agentDir: string;
	parentSessionFile?: string;
	availableToolNames: readonly string[];
};

type BootstrapOptions = {
	agentConfig: AgentConfig;
	cwd: string;
	ctx: BootstrapContext;
	model?: Model<Api>;
	tools?: string[];
};

type BootstrapResult = {
	session: AgentSession;
	warnings: string[];
};

type PromptOutcome = {
	status: Extract<SubagentStatus, "done" | "waiting" | "error" | "aborted">;
	result?: string;
	error?: string;
};

const getSkillWarnings = (
	agentConfig: AgentConfig,
	resourceLoader: DefaultResourceLoader,
): string[] => {
	const warnings: string[] = [];
	if (!agentConfig.skills) return warnings;

	const availableSkillNames = new Set(resourceLoader.getSkills().skills.map((skill) => skill.name));
	for (const skillName of agentConfig.skills) {
		if (!availableSkillNames.has(skillName)) {
			warnings.push(`Unknown skill "${skillName}" in subagent config, skipping`);
		}
	}
	return warnings;
};

export type AvailableModelIdentity = {
	provider: string;
	id: string;
};

export type ModelResolution = {
	model?: AvailableModelIdentity;
	warnings: string[];
};

const MAX_AMBIGUITY_CANDIDATES = 3;

const formatModelIdentity = (model: AvailableModelIdentity): string =>
	`${model.provider}/${model.id}`;
const formatSelector = (selector: ParsedModel): string =>
	JSON.stringify(`${selector.provider}/${selector.modelId}`);

const tokenizeModelId = (modelId: string): string[] =>
	modelId
		.toLowerCase()
		.split(/[^a-z0-9]+/i)
		.filter(Boolean);

const isFuzzyModelMatch = (selector: ParsedModel, candidate: AvailableModelIdentity): boolean => {
	if (selector.provider !== candidate.provider) return false;

	const queryTokens = tokenizeModelId(selector.modelId);
	const candidateTokens = tokenizeModelId(candidate.id);
	let candidateIndex = 0;
	for (const queryToken of queryTokens) {
		while (
			candidateIndex < candidateTokens.length &&
			!candidateTokens[candidateIndex]?.startsWith(queryToken)
		) {
			candidateIndex++;
		}
		if (candidateIndex === candidateTokens.length) return false;
		candidateIndex++;
	}
	return true;
};

const uniqueAvailableModels = (
	availableModels: readonly AvailableModelIdentity[],
): AvailableModelIdentity[] => {
	const models: AvailableModelIdentity[] = [];
	const identities = new Set<string>();
	for (const model of availableModels) {
		const identity = formatModelIdentity(model);
		if (identities.has(identity)) continue;
		identities.add(identity);
		models.push(model);
	}
	return models;
};

const formatAmbiguousCandidates = (candidates: readonly AvailableModelIdentity[]): string => {
	const formattedCandidates = candidates
		.map((candidate) => JSON.stringify(formatModelIdentity(candidate)))
		.sort();
	const visibleCandidates = formattedCandidates.slice(0, MAX_AMBIGUITY_CANDIDATES);
	const remaining = formattedCandidates.length - visibleCandidates.length;
	return `${visibleCandidates.join(", ")}${remaining > 0 ? `, … ${remaining} more` : ""}`;
};

/**
 * Resolves provider-qualified selectors against authenticated available models only.
 * Fuzzy model-id matching is case-insensitive: selector tokens are an ordered
 * subsequence and each token must prefix its aligned candidate token.
 */
export const resolveModelPreferences = (
	preferences: readonly ParsedModel[],
	availableModels: readonly AvailableModelIdentity[],
): ModelResolution => {
	const models = uniqueAvailableModels(availableModels);
	const warnings: string[] = [];
	for (const selector of preferences) {
		const exact = models.find(
			(candidate) => candidate.provider === selector.provider && candidate.id === selector.modelId,
		);
		if (exact) return { model: exact, warnings };

		const fuzzyMatches = models.filter((candidate) => isFuzzyModelMatch(selector, candidate));
		if (fuzzyMatches.length === 1) {
			const fuzzyMatch = fuzzyMatches[0];
			if (!fuzzyMatch) continue;
			warnings.push(
				`Model selector ${formatSelector(selector)} resolved to ${JSON.stringify(formatModelIdentity(fuzzyMatch))}`,
			);
			return { model: fuzzyMatch, warnings };
		}
		if (fuzzyMatches.length > 1) {
			warnings.push(
				`Model selector ${formatSelector(selector)} is ambiguous among ${formatAmbiguousCandidates(fuzzyMatches)}, skipping`,
			);
		}
	}
	return { warnings };
};

export const resolveModel = (
	agentConfig: AgentConfig,
	ctx: BootstrapContext,
): { model?: Model<Api>; warnings: string[] } => {
	const preferences = agentConfig.parsedModels?.length
		? agentConfig.parsedModels
		: agentConfig.parsedModel
			? [agentConfig.parsedModel]
			: [];
	if (preferences.length === 0) return { model: ctx.model, warnings: [] };

	const availableModels = ctx.modelRegistry.getAvailable();
	const resolution = resolveModelPreferences(preferences, availableModels);
	if (resolution.model) {
		const model = availableModels.find(
			(candidate) =>
				candidate.provider === resolution.model?.provider && candidate.id === resolution.model.id,
		);
		if (model) return { model, warnings: resolution.warnings };
	}

	return {
		model: ctx.model,
		warnings: [
			...resolution.warnings,
			`Model preferences ${preferences.map(formatSelector).join(", ")} not available, using current session model`,
		],
	};
};

export const resolveTools = (
	agentConfig: AgentConfig,
	availableToolNames: readonly string[],
): { tools: string[] | undefined; warnings: string[] } => {
	const declaredTools = agentConfig.tools;
	if (declaredTools === undefined) {
		return { tools: undefined, warnings: [] };
	}
	if (declaredTools.length === 0) {
		return { tools: [], warnings: [] };
	}

	const availableTools = new Set(availableToolNames);
	const tools = declaredTools.filter((toolName) => availableTools.has(toolName));
	const unknownTools = declaredTools.filter((toolName) => !availableTools.has(toolName));
	const warnings = unknownTools.map(
		(toolName) => `Unknown tool "${toolName}" in subagent config, skipping`,
	);

	return { tools, warnings };
};

function normalizeSessionNamePart(value: string): string {
	return value
		.replace(/[\u0000-\u001f\u007f]/g, " ")
		.replace(/\s+/g, " ")
		.trim();
}

export function formatSubagentSessionName(
	state: Pick<SubagentState, "agentConfig" | "brief" | "id">,
): string {
	const agentName = normalizeSessionNamePart(state.agentConfig.name) || "subagent";
	const brief = normalizeSessionNamePart(state.brief) || state.id;
	return `team: ${agentName} · ${brief}`;
}

const isAborted = (state: SubagentState): boolean => state.status === "aborted";

const getLastAssistantMessage = (messages: AgentMessage[]): AssistantMessage | undefined => {
	for (let i = messages.length - 1; i >= 0; i--) {
		const msg = messages[i];
		if (msg?.role === "assistant") {
			return msg as AssistantMessage;
		}
	}

	return undefined;
};

const getAssistantText = (message: AssistantMessage | undefined): string | undefined => {
	if (!message) return undefined;
	const texts: string[] = [];
	for (const part of message.content) {
		if (part.type === "text") texts.push(part.text);
	}
	return texts.length > 0 ? texts.join("\n") : undefined;
};

const getPromptOutcome = (state: SubagentState): PromptOutcome => {
	const lastAssistant = getLastAssistantMessage(state.session!.messages);
	const text = getAssistantText(lastAssistant);

	if (lastAssistant?.stopReason === "error") {
		return {
			status: "error",
			error: lastAssistant.errorMessage ?? text ?? "(no output)",
		};
	}
	if (lastAssistant?.stopReason === "aborted") {
		return {
			status: "aborted",
			error: lastAssistant.errorMessage ?? text ?? "(no output)",
		};
	}
	return {
		status: state.agentConfig.interactive ? "waiting" : "done",
		result: text ?? "(no output)",
	};
};

const DEFAULT_COMPACTION = true;
async function bootstrapSession({
	agentConfig,
	ctx,
	cwd,
	model,
	tools,
}: BootstrapOptions): Promise<BootstrapResult> {
	const { modelRegistry } = ctx;
	const { authStorage } = modelRegistry;
	const warnings: string[] = [];

	const resourceLoader = new DefaultResourceLoader({
		cwd,
		agentDir: ctx.agentDir,
		skillsOverride: agentConfig.skills
			? (base) => ({
					skills: base.skills.filter((skill) => agentConfig.skills?.includes(skill.name)),
					diagnostics: base.diagnostics,
				})
			: undefined,
		appendSystemPromptOverride: (base) =>
			agentConfig.systemPrompt.trim() ? [...base, agentConfig.systemPrompt] : base,
	});

	await resourceLoader.reload();
	warnings.push(...getSkillWarnings(agentConfig, resourceLoader));

	const settingsManager = SettingsManager.inMemory({
		compaction: { enabled: agentConfig.compaction ?? DEFAULT_COMPACTION },
	});

	const sessionManager = SessionManager.create(cwd);
	sessionManager.newSession({ parentSession: ctx.parentSessionFile });

	const result = await createAgentSession({
		cwd,
		agentDir: ctx.agentDir,
		model,
		thinkingLevel: agentConfig.thinking,
		tools,
		resourceLoader,
		sessionManager,
		settingsManager,
		authStorage,
		modelRegistry,
	});

	return { session: result.session, warnings };
}

export class SubagentSessionRunner implements SubagentRunner {
	constructor(private callbacks: SubagentRunnerCallbacks) {}

	async start(state: SubagentState, opts: StartOptions): Promise<void> {
		try {
			if (isAborted(state)) {
				return;
			}

			const modelResolution = resolveModel(state.agentConfig, opts.ctx);
			const toolResolution = resolveTools(state.agentConfig, opts.ctx.availableToolNames);
			for (const warning of [...modelResolution.warnings, ...toolResolution.warnings]) {
				opts.onWarning?.(warning);
			}

			const { session, warnings } = await bootstrapSession({
				agentConfig: state.agentConfig,
				cwd: opts.cwd,
				ctx: opts.ctx,
				model: modelResolution.model,
				tools: toolResolution.tools,
			});

			for (const warning of warnings) {
				opts.onWarning?.(warning);
			}
			if (!this.attachSpawnedSession(state, session)) {
				return;
			}

			this.attachSessionListeners(state, session);
			await this.runPromptCycle(state, state.task);
		} catch (error) {
			if (isAborted(state)) {
				return;
			}
			this.callbacks.onSettled(state, "error", {
				error: error instanceof Error ? error.message : String(error),
			});
		}
	}
	respond(state: SubagentState, message: string): void {
		void this.runPromptCycle(state, message);
	}
	abort(state: SubagentState): void {
		state.session?.abortCompaction();
		state.session?.abort().catch(() => {});
	}

	private attachSpawnedSession(state: SubagentState, session: AgentSession): boolean {
		if (!this.callbacks.isCurrent(state)) {
			session.dispose();
			return false;
		}
		state.session = session;
		if (session.model) {
			state.model = formatModelIdentity(session.model);
		}
		session.setSessionName(formatSubagentSessionName(state));
		this.callbacks.onSessionAttached(state, session.sessionManager.getSessionId());
		return true;
	}

	private attachSessionListeners(state: SubagentState, session: AgentSession): void {
		state.unsubscribe = session.subscribe((event) => {
			if (event.type === "turn_end") {
				state.turns++;
				const msg = event.message;
				if (msg.role === "assistant") {
					const assistantMsg = msg as AssistantMessage;
					state.contextTokens = assistantMsg.usage.totalTokens;
					state.model = `${assistantMsg.provider}/${assistantMsg.model}`;
				}
				this.callbacks.onProgress(state.ownerSessionId);
				return;
			}

			if (event.type === "compaction_end" && event.result?.estimatedTokensAfter !== undefined) {
				state.contextTokens = event.result.estimatedTokensAfter;
				this.callbacks.onProgress(state.ownerSessionId);
			}
		});
	}

	private async runPromptCycle(state: SubagentState, prompt: string): Promise<void> {
		if (isAborted(state)) return;

		try {
			await state.session!.prompt(prompt);
			if (isAborted(state)) return;

			const outcome = getPromptOutcome(state);
			this.callbacks.onSettled(state, outcome.status, outcome);
		} catch (err) {
			if (isAborted(state)) return;
			const error = err instanceof Error ? err.message : String(err);
			this.callbacks.onSettled(state, "error", { error });
		}
	}
}
