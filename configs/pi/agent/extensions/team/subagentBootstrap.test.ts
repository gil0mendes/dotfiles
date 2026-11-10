import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { InMemoryCredentialStore, type Provider } from "@earendil-works/pi-ai";
import * as sdk from "@earendil-works/pi-coding-agent";
import { ModelRegistry, ModelRuntime } from "@earendil-works/pi-coding-agent";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
	createChildModelRuntime,
	resolveModel,
	SubagentSessionRunner,
	type BootstrapContext,
} from "./subagentRunner";
import type { SubagentState } from "./types";
import type { AgentConfig } from "./loader/types";
import { parseModel } from "./loader/utils";
import teamExtension from "./index";

vi.mock("@earendil-works/pi-coding-agent", async (importOriginal) => {
	const original = await importOriginal<typeof sdk>();
	return { ...original, createAgentSession: vi.fn(original.createAgentSession) };
});

type ProviderConfigInput = Parameters<ModelRuntime["registerProvider"]>[1];

const directories: string[] = [];
const runtimes: ModelRuntime[] = [];

async function fixtureDirectory(): Promise<string> {
	const directory = await mkdtemp(join(process.env.TMPDIR ?? ".", "team-runtime-"));
	directories.push(directory);
	return directory;
}

async function parentRegistry(): Promise<ModelRegistry> {
	const runtime = await ModelRuntime.create({
		credentials: new InMemoryCredentialStore(),
		modelsPath: null,
		refreshOnCreate: false,
	});
	runtimes.push(runtime);
	return new ModelRegistry(runtime);
}

function context(agentDir: string, modelRegistry: ModelRegistry): BootstrapContext {
	return { agentDir, modelRegistry, availableToolNames: [] };
}

const model = {
	id: "fixture-model",
	name: "Fixture",
	reasoning: false,
	input: ["text"],
	cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
	contextWindow: 1000,
	maxTokens: 100,
} satisfies NonNullable<ProviderConfigInput["models"]>[number];

const config: AgentConfig = {
	name: "worker",
	description: "test",
	type: "agent",
	systemPrompt: "test",
	filePath: "/fixture.md",
};

afterEach(async () => {
	for (const runtime of runtimes.splice(0)) await runtime.settleCredentialWrites();
	for (const directory of directories.splice(0))
		await rm(directory, { recursive: true, force: true });
	vi.restoreAllMocks();
	vi.unstubAllEnvs();
});

describe("child model runtime", () => {
	it("loads custom models and environment-backed auth from the child agent directory", async () => {
		const directory = await fixtureDirectory();
		vi.stubEnv("TEAM_TEST_FIXTURE_KEY", "ambient-fixture");
		await writeFile(
			join(directory, "models.json"),
			JSON.stringify({
				providers: {
					filefixture: {
						baseUrl: "https://file.invalid",
						api: "openai-completions",
						apiKey: "$TEAM_TEST_FIXTURE_KEY",
						headers: { "X-Provider": "provider-value" },
						models: [{ ...model, headers: { "X-Model": "model-value" } }],
					},
				},
			}),
		);
		const child = await createChildModelRuntime(context(directory, await parentRegistry()));
		runtimes.push(child);
		const found = (await child.getAvailable()).find((entry) => entry.provider === "filefixture");
		if (!found) throw new Error("Missing file fixture model");
		expect(found.baseUrl).toBe("https://file.invalid");
		expect(await new ModelRegistry(child).getApiKeyAndHeaders(found)).toMatchObject({
			ok: true,
			apiKey: "ambient-fixture",
			headers: { "X-Provider": "provider-value", "X-Model": "model-value" },
		});
	});

	it("loads only the team extension in an isolated SDK session without prompting", async () => {
		const directory = await fixtureDirectory();
		const runtime = await ModelRuntime.create({
			credentials: new InMemoryCredentialStore(),
			modelsPath: null,
			refreshOnCreate: false,
		});
		runtimes.push(runtime);
		runtime.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			apiKey: "key",
			models: [model],
		});
		const current = (await runtime.getAvailable()).find((entry) => entry.provider === "fixture");
		if (!current) throw new Error("Missing smoke model");
		const settings = sdk.SettingsManager.inMemory();
		const loader = new sdk.DefaultResourceLoader({
			cwd: directory,
			agentDir: directory,
			settingsManager: settings,
			noExtensions: true,
			noSkills: true,
			noPromptTemplates: true,
			noThemes: true,
			noContextFiles: true,
			extensionFactories: [teamExtension],
		});
		await loader.reload();
		const original = await vi.importActual<typeof sdk>("@earendil-works/pi-coding-agent");
		const result = await original.createAgentSession({
			cwd: directory,
			agentDir: directory,
			modelRuntime: runtime,
			model: current,
			resourceLoader: loader,
			settingsManager: settings,
			sessionManager: sdk.SessionManager.inMemory(directory),
		});
		try {
			expect(result.extensionsResult.errors).toEqual([]);
			expect(result.session.model?.provider).toBe("fixture");
			expect(result.session.model?.id).toBe(model.id);
			expect(result.session.getActiveToolNames()).toContain("team_spawn");
		} finally {
			result.session.dispose();
		}
	});

	it("replays legacy registration, file overlays and runtime keys without modifying the parent or auth file", async () => {
		const directory = await fixtureDirectory();
		const authFile = join(directory, "auth.json");
		const authContents = JSON.stringify({ fixture: { type: "api_key", key: "stored-fixture" } });
		await writeFile(authFile, authContents);
		await writeFile(
			join(directory, "models.json"),
			JSON.stringify({
				providers: {
					fixture: {
						baseUrl: "https://overlay.invalid",
						headers: { "X-Overlay": "overlay" },
						modelOverrides: { "fixture-model": { maxTokens: 42 } },
					},
				},
			}),
		);
		const registry = await parentRegistry();
		const streamSimple: NonNullable<ProviderConfigInput["streamSimple"]> = () => {
			throw new Error("transport must not be called");
		};
		const oauth: NonNullable<ProviderConfigInput["oauth"]> = {
			name: "fixture oauth",
			login: async () => {
				throw new Error("unexpected login");
			},
			refreshToken: async (credentials) => credentials,
			getApiKey: (credentials) => credentials.access,
		};
		const registration: ProviderConfigInput = {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			apiKey: "configured-fixture",
			models: [model],
			streamSimple,
			oauth,
		};
		registry.registerProvider("fixture", registration);
		const parent = registry.getRegisteredProviderConfig("fixture");
		// Use the public runtime interface to set the parent override.
		const parentRuntime = runtimes[0];
		if (!parentRuntime) throw new Error("Missing parent runtime");
		await parentRuntime.setRuntimeApiKey("fixture", "runtime-fixture");
		const child = await createChildModelRuntime(context(directory, registry));
		runtimes.push(child);
		const childModel = (await child.getAvailable()).find((entry) => entry.provider === "fixture");
		expect(childModel?.baseUrl).toBe("https://base.invalid"); // Extension base URL takes precedence.
		expect(childModel?.maxTokens).toBe(42);
		if (!childModel) throw new Error("Missing child fixture model");
		expect(await new ModelRegistry(child).getApiKeyAndHeaders(childModel)).toMatchObject({
			ok: true,
			headers: { "X-Overlay": "overlay" },
		});
		expect(child.getRegisteredProviderConfig("fixture")?.streamSimple).toBe(streamSimple);
		expect(child.getRegisteredProviderConfig("fixture")?.oauth).toBe(oauth);
		expect((await child.getAuth("fixture"))?.auth.apiKey).toBe("runtime-fixture");
		expect(child.getProviderAuthStatus("fixture").source).toBe("runtime");
		expect(registry.getRegisteredProviderConfig("fixture")).toBe(parent);
		expect(await readFile(authFile, "utf8")).toBe(authContents);
	});

	it("replays native provider registrations without cloning composed providers", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		const resolve = vi.fn(async () => ({ auth: { apiKey: "native-fixture-key" } }));
		const native: Provider = {
			id: "native-fixture",
			name: "Native fixture",
			auth: { apiKey: { name: "Native key", resolve } },
			getModels: () => [
				{
					...model,
					provider: "native-fixture",
					api: "openai-completions",
					baseUrl: "https://native.invalid",
				},
			],
			stream: () => {
				throw new Error("transport must not be called");
			},
			streamSimple: () => {
				throw new Error("transport must not be called");
			},
		};
		registry.registerProvider(native);
		const child = await createChildModelRuntime(context(directory, registry));
		runtimes.push(child);
		expect(child.getRegisteredNativeProvider(native.id)).toBe(native);
		expect((await child.getAuth(native.id))?.auth.apiKey).toBe("native-fixture-key");
		expect(resolve).toHaveBeenCalled();
		await vi.waitFor(async () => {
			expect(await child.getAvailable()).toEqual(
				expect.arrayContaining([expect.objectContaining({ provider: native.id, id: model.id })]),
			);
		});
	});

	it("keeps synthetic file OAuth refreshable through the replayed callback", async () => {
		const directory = await fixtureDirectory();
		const authFile = join(directory, "auth.json");
		await writeFile(
			authFile,
			JSON.stringify({
				fixture: {
					type: "oauth",
					access: "initial",
					refresh: "refresh-initial",
					expires: 0,
				},
			}),
		);
		let refreshes = 0;
		let now = Date.now();
		vi.spyOn(Date, "now").mockImplementation(() => now);
		const oauth: NonNullable<ProviderConfigInput["oauth"]> = {
			name: "fixture",
			login: async () => {
				throw new Error("unexpected login");
			},
			refreshToken: async () => {
				refreshes++;
				return {
					access: `access-${refreshes}`,
					refresh: `refresh-${refreshes}`,
					expires: Date.now() + 360_000,
				};
			},
			getApiKey: (credentials) => credentials.access,
		};
		const registry = await parentRegistry();
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			models: [model],
			oauth,
		});
		const child = await createChildModelRuntime(context(directory, registry));
		runtimes.push(child);
		const first = await child.getAuth("fixture", { minOAuthValidityMs: 60_000 });
		const firstCount = refreshes;
		now += 310_000;
		const second = await child.getAuth("fixture", { minOAuthValidityMs: 60_000 });
		await child.settleCredentialWrites();
		expect(first?.auth.apiKey).toMatch(/^access-/);
		expect(second?.auth.apiKey).toMatch(/^access-/);
		expect(refreshes).toBeGreaterThan(firstCount);
		expect(JSON.parse(await readFile(authFile, "utf8")).fixture).toMatchObject({
			type: "oauth",
			refresh: `refresh-${refreshes}`,
		});
		expect(child.getProviderAuthStatus("fixture").source).toBe("stored");
	});

	it("fails without exposing a runtime key when the parent cannot resolve it", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			models: [model],
		});
		const runtime = runtimes[0];
		if (!runtime) throw new Error("Missing parent runtime");
		await runtime.setRuntimeApiKey("fixture", "secret-fixture");
		vi.spyOn(registry, "getProviderAuth").mockRejectedValue(new Error("secret-fixture"));
		await expect(createChildModelRuntime(context(directory, registry))).rejects.toThrow(
			'Failed to inherit runtime API key for provider "fixture"',
		);
	});

	it("settles a failed runtime-key transfer without invoking the SDK", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			models: [model],
		});
		const runtime = runtimes[0];
		if (!runtime) throw new Error("Missing parent runtime");
		await runtime.setRuntimeApiKey("fixture", "secret-fixture");
		vi.spyOn(registry, "getProviderAuth").mockRejectedValue(new Error("secret-fixture"));
		const state: SubagentState = {
			id: "fixture",
			agentConfig: config,
			task: "never sent",
			brief: "test",
			status: "running",
			ownerSessionId: "owner",
			startedAt: 0,
			session: null,
			turns: 0,
			contextTokens: 0,
		};
		const onSettled = vi.fn();
		const createSpy = vi.mocked(sdk.createAgentSession);
		createSpy.mockClear();
		const runner = new SubagentSessionRunner({
			isCurrent: () => true,
			onProgress: vi.fn(),
			onSessionAttached: vi.fn(),
			onSettled,
		});
		await runner.start(state, { cwd: directory, ctx: context(directory, registry) });
		expect(createSpy).not.toHaveBeenCalled();
		expect(onSettled).toHaveBeenCalledWith(state, "error", {
			error: 'Failed to inherit runtime API key for provider "fixture"',
		});
	});

	it("rejects a stored-to-environment credential downgrade for the current model", async () => {
		const directory = await fixtureDirectory();
		vi.stubEnv("ANTHROPIC_API_KEY", "ambient-fixture");
		const credentials = new InMemoryCredentialStore();
		await credentials.modify("anthropic", async () => ({ type: "api_key", key: "stored-fixture" }));
		const runtime = await ModelRuntime.create({
			credentials,
			modelsPath: null,
			refreshOnCreate: false,
		});
		runtimes.push(runtime);
		const registry = new ModelRegistry(runtime);
		await runtime.getAvailable();
		const current = registry.getAll().find((entry) => entry.provider === "anthropic");
		if (!current) throw new Error("Missing anthropic fixture");
		expect(registry.getProviderAuthStatus("anthropic").source).toBe("stored");
		const state: SubagentState = {
			id: "fixture",
			agentConfig: config,
			task: "never sent",
			brief: "test",
			status: "running",
			ownerSessionId: "owner",
			startedAt: 0,
			session: null,
			turns: 0,
			contextTokens: 0,
		};
		const onSettled = vi.fn();
		const createSpy = vi.mocked(sdk.createAgentSession);
		createSpy.mockClear();
		const runner = new SubagentSessionRunner({
			isCurrent: () => true,
			onProgress: vi.fn(),
			onSessionAttached: vi.fn(),
			onSettled,
		});
		await runner.start(state, {
			cwd: directory,
			ctx: { ...context(directory, registry), model: current },
		});
		expect(createSpy).not.toHaveBeenCalled();
		expect(onSettled).toHaveBeenCalledWith(state, "error", {
			error: expect.stringMatching(/Stored credentials .* unavailable/),
		});
	});

	it("rejects a models.json key fallback when the parent uses stored credentials", async () => {
		const directory = await fixtureDirectory();
		await writeFile(
			join(directory, "models.json"),
			JSON.stringify({
				providers: { fixture: { apiKey: "configured-fixture" } },
			}),
		);
		const credentials = new InMemoryCredentialStore();
		await credentials.modify("fixture", async () => ({ type: "api_key", key: "stored-fixture" }));
		const runtime = await ModelRuntime.create({
			credentials,
			modelsPath: null,
			refreshOnCreate: false,
		});
		runtimes.push(runtime);
		const registry = new ModelRegistry(runtime);
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			models: [model],
		});
		await runtime.getAvailable();
		const current = registry.find("fixture", model.id);
		if (!current) throw new Error("Missing fixture model");
		await vi.waitFor(async () => {
			await runtime.getAvailable();
			expect(registry.getProviderAuthStatus("fixture").source).toBe("stored");
		});
		const child = await createChildModelRuntime({
			...context(directory, registry),
			model: current,
		});
		runtimes.push(child);
		await vi.waitFor(async () => {
			expect(
				(await child.getAvailable()).some(
					(entry) => entry.provider === "fixture" && entry.id === model.id,
				),
			).toBe(true);
		});
		expect(child.getProviderAuthStatus("fixture").source).toBe("models_json_key");
		expect((await child.getAuth("fixture"))?.auth.apiKey).toBe("configured-fixture");

		const state: SubagentState = {
			id: "fixture",
			agentConfig: config,
			task: "never sent",
			brief: "test",
			status: "running",
			ownerSessionId: "owner",
			startedAt: 0,
			session: null,
			turns: 0,
			contextTokens: 0,
		};
		const onSettled = vi.fn();
		const createSpy = vi.mocked(sdk.createAgentSession);
		createSpy.mockClear();
		const runner = new SubagentSessionRunner({
			isCurrent: () => true,
			onProgress: vi.fn(),
			onSessionAttached: vi.fn(),
			onSettled,
		});
		await runner.start(state, {
			cwd: directory,
			ctx: { ...context(directory, registry), model: current },
		});
		expect(createSpy).not.toHaveBeenCalled();
		expect(onSettled).toHaveBeenCalledWith(state, "error", {
			error: expect.stringMatching(/Stored credentials .* unavailable/),
		});

		const preference = parseModel("fixture mod");
		if (!preference) throw new Error("Missing test selector");
		const explicitState: SubagentState = {
			...state,
			status: "running",
			agentConfig: { ...config, parsedModels: [preference] },
		};
		const onWarning = vi.fn();
		vi.spyOn(sdk.SessionManager, "create").mockImplementation((cwd) =>
			sdk.SessionManager.inMemory(cwd),
		);
		const original = await vi.importActual<typeof sdk>("@earendil-works/pi-coding-agent");
		createSpy.mockImplementationOnce(async (options) => {
			const result = await original.createAgentSession(options);
			vi.spyOn(result.session, "prompt").mockResolvedValue(undefined);
			return result;
		});
		await runner.start(explicitState, {
			cwd: directory,
			ctx: { ...context(directory, registry), model: current },
			onWarning,
		});
		expect(createSpy).toHaveBeenCalledOnce();
		const selected = createSpy.mock.calls[0]?.[0]?.model;
		expect(selected).toMatchObject({ provider: "fixture", id: model.id });
		expect(selected).not.toBe(current);
		expect(explicitState.model).toBe("fixture/fixture-model");
		expect(onWarning).toHaveBeenCalledWith(
			'Model selector "fixture mod" resolved to "fixture/fixture-model"',
		);
		explicitState.session?.dispose();
	});

	it("does not start a session when aborted during runtime creation", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		const original = ModelRuntime.create.bind(ModelRuntime);
		let unblock: (() => void) | undefined;
		const gate = new Promise<void>((resolve) => {
			unblock = resolve;
		});
		let ready: (() => void) | undefined;
		const started = new Promise<void>((resolve) => {
			ready = resolve;
		});
		vi.spyOn(ModelRuntime, "create").mockImplementationOnce(async (options) => {
			const runtime = await original(options);
			runtimes.push(runtime);
			ready?.();
			await gate;
			return runtime;
		});
		const state: SubagentState = {
			id: "fixture",
			agentConfig: config,
			task: "never sent",
			brief: "test",
			status: "running",
			ownerSessionId: "owner",
			startedAt: 0,
			session: null,
			turns: 0,
			contextTokens: 0,
		};
		const onSettled = vi.fn();
		const createSpy = vi.mocked(sdk.createAgentSession);
		createSpy.mockClear();
		const runner = new SubagentSessionRunner({
			isCurrent: () => true,
			onProgress: vi.fn(),
			onSessionAttached: vi.fn(),
			onSettled,
		});
		const running = runner.start(state, { cwd: directory, ctx: context(directory, registry) });
		await started;
		state.status = "aborted";
		unblock?.();
		await running;
		expect(createSpy).not.toHaveBeenCalled();
		expect(onSettled).not.toHaveBeenCalled();
	});

	it("passes the reconstructed runtime to the SDK and disposes a session aborted before attachment", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			apiKey: "key",
			models: [model],
		});
		const current = registry.find("fixture", model.id);
		if (!current) throw new Error("Missing model fixture");
		const state: SubagentState = {
			id: "fixture",
			agentConfig: { ...config, thinking: "low", compaction: false, tools: [] },
			task: "never sent",
			brief: "test",
			status: "running",
			ownerSessionId: "owner",
			startedAt: 0,
			session: null,
			turns: 0,
			contextTokens: 0,
		};
		vi.spyOn(sdk.SessionManager, "create").mockImplementation((cwd) =>
			sdk.SessionManager.inMemory(cwd),
		);
		const original = await vi.importActual<typeof sdk>("@earendil-works/pi-coding-agent");
		let unblock: (() => void) | undefined;
		const gate = new Promise<void>((resolve) => {
			unblock = resolve;
		});
		let ready: (() => void) | undefined;
		const started = new Promise<void>((resolve) => {
			ready = resolve;
		});
		let dispose: ReturnType<typeof vi.spyOn> | undefined;
		let prompt: ReturnType<typeof vi.spyOn> | undefined;
		const createSpy = vi.mocked(sdk.createAgentSession).mockImplementationOnce(async (options) => {
			const result = await original.createAgentSession(options);
			dispose = vi.spyOn(result.session, "dispose");
			prompt = vi.spyOn(result.session, "prompt");
			ready?.();
			await gate;
			return result;
		});
		const onSettled = vi.fn();
		const onSessionAttached = vi.fn();
		const runner = new SubagentSessionRunner({
			isCurrent: () => true,
			onProgress: vi.fn(),
			onSessionAttached,
			onSettled,
		});
		const running = runner.start(state, {
			cwd: directory,
			ctx: {
				...context(directory, registry),
				model: current,
				parentSessionFile: "/synthetic-parent.jsonl",
			},
		});
		await started;
		const options = createSpy.mock.calls[0]?.[0];
		expect(options?.modelRuntime).toBeInstanceOf(ModelRuntime);
		expect(options).not.toHaveProperty("authStorage");
		expect(options).not.toHaveProperty("modelRegistry");
		expect(options).toMatchObject({
			cwd: directory,
			agentDir: directory,
			model: current,
			thinkingLevel: "low",
			tools: [],
		});
		expect(options?.settingsManager?.getCompactionEnabled()).toBe(false);
		expect(options?.resourceLoader?.getAppendSystemPrompt()).toContain(config.systemPrompt);
		expect(options?.sessionManager?.getHeader()?.parentSession).toBe("/synthetic-parent.jsonl");
		state.status = "aborted";
		unblock?.();
		await running;
		expect(dispose).toHaveBeenCalledOnce();
		expect(prompt).not.toHaveBeenCalled();
		expect(onSessionAttached).not.toHaveBeenCalled();
		expect(onSettled).not.toHaveBeenCalled();
	});

	it("forwards session message events only while the attached state is current", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			apiKey: "key",
			models: [model],
		});
		const current = registry.find("fixture", model.id);
		if (!current) throw new Error("Missing model fixture");
		const state: SubagentState = {
			id: "fixture",
			agentConfig: config,
			task: "task",
			brief: "",
			status: "running",
			ownerSessionId: "owner",
			startedAt: 0,
			session: null,
			turns: 0,
			contextTokens: 0,
		};
		vi.spyOn(sdk.SessionManager, "create").mockImplementation((cwd) =>
			sdk.SessionManager.inMemory(cwd),
		);
		const original = await vi.importActual<typeof sdk>("@earendil-works/pi-coding-agent");
		let listener: sdk.AgentSessionEventListener | undefined;
		let release: (() => void) | undefined;
		let ready: (() => void) | undefined;
		const started = new Promise<void>((resolve) => {
			ready = resolve;
		});
		const gate = new Promise<void>((resolve) => {
			release = resolve;
		});
		vi.mocked(sdk.createAgentSession).mockImplementationOnce(async (options) => {
			const result = await original.createAgentSession(options);
			vi.spyOn(result.session, "subscribe").mockImplementation((callback) => {
				listener = callback;
				return () => {};
			});
			vi.spyOn(result.session, "prompt").mockImplementation(async () => {
				ready?.();
				await gate;
			});
			return result;
		});
		let isCurrent = true;
		const onConversationEvent = vi.fn();
		const onConversationError = vi.fn();
		const onProgress = vi.fn();
		const runner = new SubagentSessionRunner({
			isCurrent: () => isCurrent,
			onConversationEvent,
			onConversationError,
			onProgress,
			onSessionAttached: vi.fn(),
			onSettled: vi.fn(),
		});
		const running = runner.start(state, {
			cwd: directory,
			ctx: { ...context(directory, registry), model: current },
		});
		await started;
		if (!listener) throw new Error("Session did not subscribe");
		const event = {
			type: "message_start" as const,
			message: { role: "user" as const, content: "task", timestamp: 0 },
		};
		listener(event);
		expect(onConversationEvent).toHaveBeenCalledWith(state, event);
		onConversationEvent.mockImplementationOnce(() => {
			throw new Error("private payload");
		});
		listener({ type: "turn_end", message: event.message, toolResults: [] });
		expect(onConversationError).toHaveBeenCalledWith(state);
		expect(onProgress).toHaveBeenCalledWith(state);
		expect(state.turns).toBe(1);
		isCurrent = false;
		listener(event);
		expect(onConversationEvent).toHaveBeenCalledTimes(2);
		release?.();
		await running;
		state.session?.dispose();
	});

	it("rejects a parent-only model when no child catalog reproduces it", async () => {
		const directory = await fixtureDirectory();
		const registry = await parentRegistry();
		registry.registerProvider("fixture", {
			baseUrl: "https://base.invalid",
			api: "openai-completions",
			apiKey: "key",
			models: [model],
		});
		const physical = registry.find("fixture", model.id);
		if (!physical) throw new Error("Missing model fixture");
		registry.registerVirtualModel({
			provider: "fixture",
			id: "virtual",
			name: "Virtual",
			route: () => ({ model: physical, thinkingLevel: "off" }),
		});
		const current = registry.find("fixture", "virtual");
		if (!current) throw new Error("Missing virtual fixture");
		const child = await createChildModelRuntime(context(directory, registry));
		runtimes.push(child);
		const availableModels = await child.getAvailable();
		expect(() => resolveModel(config, { model: current, availableModels })).toThrow(
			/virtual models cannot be inherited/,
		);
	});
});
