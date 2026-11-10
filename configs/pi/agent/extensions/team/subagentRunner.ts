import { join } from "node:path";
import type { Api, AssistantMessage, Model } from "@earendil-works/pi-ai";
import type { SettledSubagentStatus, SubagentState, SubagentStatus } from "./types";
import {
	createAgentSession,
	DefaultResourceLoader,
	ModelRuntime,
	SessionManager,
	SettingsManager,
	type AgentSession,
	type AgentSessionEvent,
	type ModelRegistry,
} from "@earendil-works/pi-coding-agent";
import type { AgentConfig, ParsedModel } from "./loader/types";
import { selectAvailableModel } from "./modelSelection";
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
	onProgress: (state: SubagentState) => void;
	onConversationEvent?: (state: SubagentState, event: AgentSessionEvent) => void;
	onConversationError?: (state: SubagentState) => void;
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
	modelRuntime: ModelRuntime;
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
	name?: string;
};

export type ModelResolution = {
	model?: AvailableModelIdentity;
	warnings: string[];
};

const MAX_AMBIGUITY_CANDIDATES = 3;

const formatModelIdentity = (model: AvailableModelIdentity): string =>
	`${model.provider}/${model.id}`;
const formatSelector = (selector: ParsedModel): string => JSON.stringify(selector.raw);

const formatAmbiguousCandidates = (candidates: readonly AvailableModelIdentity[]): string => {
	const formattedCandidates = candidates
		.map(formatModelIdentity)
		.sort()
		.map((identity) => JSON.stringify(identity));
	const visibleCandidates = formattedCandidates.slice(0, MAX_AMBIGUITY_CANDIDATES);
	const remaining = formattedCandidates.length - visibleCandidates.length;
	return `${visibleCandidates.join(", ")}${remaining > 0 ? `, … ${remaining} more` : ""}`;
};

export const resolveModelPreferences = <T extends AvailableModelIdentity>(
	preferences: readonly ParsedModel[],
	availableModels: readonly T[],
): { model?: T; warnings: string[] } => {
	const warnings: string[] = [];
	for (const selector of preferences) {
		const result = selectAvailableModel(selector, availableModels);
		if (result.kind === "unavailable") {
			warnings.push(`Model selector ${formatSelector(selector)} is unavailable, skipping`);
			continue;
		}
		if (result.kind === "ambiguous") {
			warnings.push(
				`Model selector ${formatSelector(selector)} is ambiguous among ${formatAmbiguousCandidates(result.candidates)}, skipping`,
			);
			continue;
		}
		const { model } = result;
		const isExact =
			selector.kind === "qualified"
				? selector.provider === model.provider && selector.modelId === model.id
				: selector.query.trim().toLowerCase() === model.id.trim().toLowerCase();
		if (!isExact) {
			warnings.push(
				`Model selector ${formatSelector(selector)} resolved to ${JSON.stringify(formatModelIdentity(model))}`,
			);
		}
		return { model, warnings };
	}
	return { warnings };
};

export const resolveModel = (
	agentConfig: AgentConfig,
	input: { model?: Model<Api>; availableModels: readonly Model<Api>[] },
): { model?: Model<Api>; warnings: string[]; usedCurrentModel: boolean } => {
	const preferences = agentConfig.parsedModels?.length
		? agentConfig.parsedModels
		: agentConfig.parsedModel
			? [agentConfig.parsedModel]
			: [];
	const { availableModels } = input;
	const resolution = resolveModelPreferences(preferences, availableModels);
	if (resolution.model) {
		return { model: resolution.model, warnings: resolution.warnings, usedCurrentModel: false };
	}

	const current =
		input.model &&
		availableModels.find(
			(candidate) =>
				candidate.provider === input.model?.provider && candidate.id === input.model.id,
		);
	if (input.model && !current) {
		throw new Error(
			`Current model ${JSON.stringify(formatModelIdentity(input.model))} is unavailable in the child runtime; private credentials or virtual models cannot be inherited`,
		);
	}
	if (!current && availableModels.length === 0) {
		throw new Error(
			"No authenticated models are available in the child runtime; configure a provider and credentials",
		);
	}
	return {
		model: current,
		usedCurrentModel: Boolean(current),
		warnings: preferences.length
			? [
					...resolution.warnings,
					`Model preferences ${preferences.map(formatSelector).join(", ")} not available, using current session model`,
				]
			: resolution.warnings,
	};
};

export async function createChildModelRuntime(ctx: BootstrapContext): Promise<ModelRuntime> {
	let runtime: ModelRuntime;
	try {
		runtime = await ModelRuntime.create({
			authPath: join(ctx.agentDir, "auth.json"),
			modelsPath: join(ctx.agentDir, "models.json"),
			allowModelNetwork: false,
		});
	} catch {
		throw new Error("Failed to initialize child model runtime");
	}
	if (ctx.modelRegistry.getError() || runtime.getError()) {
		throw new Error("Invalid parent or child model configuration");
	}
	const registeredIds = ctx.modelRegistry.getRegisteredProviderIds();
	for (const id of registeredIds) {
		try {
			const native = ctx.modelRegistry.getRegisteredNativeProvider(id);
			const config = ctx.modelRegistry.getRegisteredProviderConfig(id);
			if (native) runtime.registerNativeProvider(native);
			else if (config) runtime.registerProvider(id, config);
			else throw new Error("Missing registration");
		} catch {
			throw new Error(`Failed to replay provider registration ${JSON.stringify(id)}`);
		}
	}
	const providerIds = new Set([
		...registeredIds,
		...ctx.modelRegistry.getAll().map((model) => model.provider),
		...(ctx.model ? [ctx.model.provider] : []),
	]);
	for (const id of providerIds) {
		if (ctx.modelRegistry.getProviderAuthStatus(id).source !== "runtime") continue;
		try {
			const auth = await ctx.modelRegistry.getProviderAuth(id);
			if (!auth?.auth.apiKey) throw new Error("Missing runtime API key");
			await runtime.setRuntimeApiKey(id, auth.auth.apiKey);
		} catch {
			throw new Error(`Failed to inherit runtime API key for provider ${JSON.stringify(id)}`);
		}
	}
	if (runtime.getError()) {
		throw new Error("Invalid child model configuration after provider registration");
	}
	return runtime;
}

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
	modelRuntime,
	tools,
}: BootstrapOptions): Promise<BootstrapResult> {
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
		modelRuntime,
	});

	if (result.modelFallbackMessage) warnings.push(result.modelFallbackMessage);
	return { session: result.session, warnings };
}

export class SubagentSessionRunner implements SubagentRunner {
	constructor(private callbacks: SubagentRunnerCallbacks) {}

	async start(state: SubagentState, opts: StartOptions): Promise<void> {
		try {
			if (isAborted(state)) {
				return;
			}

			const modelRuntime = await createChildModelRuntime(opts.ctx);
			if (isAborted(state) || !this.callbacks.isCurrent(state)) return;
			let availableModels: readonly Model<Api>[];
			try {
				availableModels = await modelRuntime.getAvailable();
			} catch {
				throw new Error("Failed to resolve child model availability");
			}
			if (isAborted(state) || !this.callbacks.isCurrent(state)) return;
			if (modelRuntime.getError()) throw new Error("Failed to resolve child model availability");
			const modelResolution = resolveModel(state.agentConfig, {
				model: opts.ctx.model,
				availableModels,
			});
			if (
				opts.ctx.model &&
				modelResolution.usedCurrentModel &&
				opts.ctx.modelRegistry.getProviderAuthStatus(opts.ctx.model.provider).source === "stored" &&
				!["stored", "runtime"].includes(
					modelRuntime.getProviderAuthStatus(opts.ctx.model.provider).source ?? "",
				)
			) {
				throw new Error(
					`Stored credentials for current model ${JSON.stringify(formatModelIdentity(opts.ctx.model))} are unavailable in the child runtime`,
				);
			}
			const toolResolution = resolveTools(state.agentConfig, opts.ctx.availableToolNames);
			for (const warning of [...modelResolution.warnings, ...toolResolution.warnings]) {
				opts.onWarning?.(warning);
			}

			const { session, warnings } = await bootstrapSession({
				agentConfig: state.agentConfig,
				cwd: opts.cwd,
				ctx: opts.ctx,
				model: modelResolution.model,
				modelRuntime,
				tools: toolResolution.tools,
			});

			if (!this.attachSpawnedSession(state, session)) {
				return;
			}
			for (const warning of warnings) {
				opts.onWarning?.(warning);
			}

			this.attachSessionListeners(state, session);
			await this.runPromptCycle(state, state.task);
		} catch (error) {
			if (isAborted(state) || !this.callbacks.isCurrent(state)) {
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
		if (isAborted(state) || !this.callbacks.isCurrent(state)) {
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
			if (!this.callbacks.isCurrent(state) || state.session !== session) return;
			try {
				this.callbacks.onConversationEvent?.(state, event);
			} catch {
				try {
					this.callbacks.onConversationError?.(state);
				} catch {
					/* Observability is non-critical. */
				}
			}
			if (event.type === "turn_end") {
				state.turns++;
				const msg = event.message;
				if (msg.role === "assistant") {
					const assistantMsg = msg as AssistantMessage;
					state.contextTokens = assistantMsg.usage.totalTokens;
					state.model = `${assistantMsg.provider}/${assistantMsg.model}`;
				}
				this.callbacks.onProgress(state);
				return;
			}

			if (event.type === "compaction_end" && event.result?.estimatedTokensAfter !== undefined) {
				state.contextTokens = event.result.estimatedTokensAfter;
				this.callbacks.onProgress(state);
			}
		});
	}

	private async runPromptCycle(state: SubagentState, prompt: string): Promise<void> {
		const session = state.session;
		if (isAborted(state) || !this.callbacks.isCurrent(state) || !session) return;

		try {
			await session.prompt(prompt);
			if (isAborted(state) || !this.callbacks.isCurrent(state) || state.session !== session) return;

			const outcome = getPromptOutcome(state);
			this.callbacks.onSettled(state, outcome.status, outcome);
		} catch (err) {
			if (isAborted(state) || !this.callbacks.isCurrent(state) || state.session !== session) return;
			const error = err instanceof Error ? err.message : String(err);
			this.callbacks.onSettled(state, "error", { error });
		}
	}
}
