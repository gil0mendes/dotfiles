import { InMemoryCredentialStore } from "@earendil-works/pi-ai";
import {
	createEventBus,
	ModelRegistry,
	ModelRuntime,
	type AgentSession,
	type AgentSessionEvent,
} from "@earendil-works/pi-coding-agent";
import { beforeAll, describe, expect, it, vi } from "vitest";
import type { SubagentState } from "./types";
import {
	Conversation,
	CONVERSATION_RECENT_MAX_COUNT,
	CONVERSATION_RECENT_PER_OWNER,
} from "./conversation";
import {
	isTeamWarningPayload,
	isTeamRuntimeChangedPayload,
	TEAM_RUNTIME_CHANGED_EVENT,
	TEAM_WARNING_EVENT,
} from "./events";
import type { AgentConfig } from "./loader/types";
import { Runtime } from "./runtime";

const agentConfig: AgentConfig = {
	name: "worker",
	type: "agent",
	description: "Test worker",
	systemPrompt: "",
	filePath: "/agents/worker.md",
};

let modelRegistry: ModelRegistry;
beforeAll(async () => {
	modelRegistry = new ModelRegistry(
		await ModelRuntime.create({
			credentials: new InMemoryCredentialStore(),
			modelsPath: null,
			refreshOnCreate: false,
		}),
	);
});

describe("conversation runtime", () => {
	it("isolates capture and retention errors from progress, result delivery and disposal", () => {
		let emit: ((event: AgentSessionEvent) => void) | undefined;
		let settle: (() => void) | undefined;
		const dispose = vi.fn();
		const deliveries: string[] = [];
		const warnings: string[] = [];
		const events = createEventBus();
		events.on(TEAM_WARNING_EVENT, (payload) => {
			if (isTeamWarningPayload(payload)) warnings.push(payload.message);
		});
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					state.session = { dispose } as unknown as AgentSession;
					emit = (event) => callbacks.onConversationEvent?.(state, event);
					settle = () => callbacks.onSettled(state, "done", { result: "delivered" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		runtime.activateSession({
			cwd: "/",
			sessionId: "owner",
			sendMessage: (message) => deliveries.push(String(message.content)),
			isIdle: () => true,
			events,
		});
		const id = runtime.spawn(agentConfig, "task", "/", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/",
			availableToolNames: [],
		});
		const capture = vi.spyOn(Conversation.prototype, "capture").mockImplementation(() => {
			throw new Error("SECRET");
		});
		const snapshot = vi.spyOn(Conversation.prototype, "snapshot").mockImplementation(() => {
			throw new Error("SECRET");
		});
		try {
			emit?.({
				type: "message_start",
				message: { role: "user", content: "private", timestamp: 1 },
			});
			settle?.();
			expect(deliveries).toEqual([expect.stringContaining("delivered")]);
			expect(dispose).toHaveBeenCalledOnce();
			expect(runtime.getActiveSummariesForOwner("owner")).toEqual([]);
			expect(warnings).toHaveLength(2);
			expect(warnings.join()).not.toContain("SECRET");
			expect(id).toBeTruthy();
		} finally {
			capture.mockRestore();
			snapshot.mockRestore();
		}
	});

	it("disposes a waiting agent even when recent capture fails during done", () => {
		let settle: (() => void) | undefined;
		const dispose = vi.fn();
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					state.session = { dispose } as unknown as AgentSession;
					settle = () => callbacks.onSettled(state, "waiting", { result: "question" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const id = runtime.spawn(agentConfig, "task", "/", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/",
			availableToolNames: [],
		});
		settle?.();
		const snapshot = vi.spyOn(Conversation.prototype, "snapshot").mockImplementation(() => {
			throw new Error("private");
		});
		try {
			expect(runtime.done(id, "owner")).toEqual({});
			expect(dispose).toHaveBeenCalledOnce();
			expect(runtime.getActiveSummariesForOwner("owner")).toEqual([]);
		} finally {
			snapshot.mockRestore();
		}
	});

	it("clears no-echo pending prompts on failed cycles", () => {
		let emit: ((event: AgentSessionEvent) => void) | undefined;
		let settle: (() => void) | undefined;
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					state.session = { dispose: () => {} } as unknown as AgentSession;
					emit = (event) => callbacks.onConversationEvent?.(state, event);
					settle = () => callbacks.onSettled(state, "error", { error: "failed" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const id = runtime.spawn(agentConfig, "/command", "/", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/",
			availableToolNames: [],
		});
		const endCycle = vi.spyOn(Conversation.prototype, "endPromptCycle");
		try {
			settle?.();
			expect(endCycle).toHaveBeenCalledOnce();
			emit?.({ type: "message_start", message: { role: "user", content: "late", timestamp: 1 } });
			expect(runtime.getConversationForOwner("owner", id)).toMatchObject({
				agent: { status: "error" },
				items: [expect.objectContaining({ text: "/command" })],
			});
		} finally {
			endCycle.mockRestore();
		}
	});

	it("coalesces authorized subscriptions and cleans them on same-owner rebind", () => {
		vi.useFakeTimers();
		try {
			let emit: ((event: AgentSessionEvent) => void) | undefined;
			const runtime = new Runtime({
				createRunner: (callbacks) => ({
					start: (state) => {
						emit = (event) => callbacks.onConversationEvent?.(state, event);
						return Promise.resolve();
					},
					respond: () => {},
					abort: () => {},
				}),
			});
			const bind = (sessionId: string) => ({
				cwd: "/",
				sessionId,
				sendMessage: () => {},
				isIdle: () => true,
				events: createEventBus(),
			});
			runtime.activateSession(bind("owner"));
			runtime.activateSession(bind("foreign"));
			const id = runtime.spawn(agentConfig, "task", "/", "owner", {
				model: undefined,
				modelRegistry,
				agentDir: "/",
				availableToolNames: [],
			});
			const first = vi.fn();
			const second = vi.fn();
			const denied = vi.fn();
			const closeFirst = runtime.subscribeConversation("owner", id, first);
			const closeSecond = runtime.subscribeConversation("owner", id, second);
			runtime.subscribeConversation("foreign", id, denied);
			runtime.subscribeConversation("owner", "missing", denied);
			const send = (text: string) =>
				emit?.({ type: "message_start", message: { role: "user", content: text, timestamp: 1 } });
			send("one");
			send("two");
			vi.advanceTimersByTime(1);
			expect(first).toHaveBeenCalledTimes(1);
			expect(second).toHaveBeenCalledTimes(1);
			expect(denied).not.toHaveBeenCalled();
			closeFirst();
			closeFirst();
			send("three");
			vi.advanceTimersByTime(1);
			expect(first).toHaveBeenCalledTimes(1);
			expect(second).toHaveBeenCalledTimes(2);
			send("queued");
			runtime.activateSession(bind("owner"));
			const rebound = vi.fn();
			runtime.subscribeConversation("owner", id, rebound);
			vi.advanceTimersByTime(1);
			expect(rebound).not.toHaveBeenCalled();
			send("after rebind");
			vi.advanceTimersByTime(1);
			expect(second).toHaveBeenCalledTimes(2);
			expect(rebound).toHaveBeenCalledTimes(1);
			expect(runtime.getConversationForOwner("owner", id)?.items.length).toBeGreaterThan(0);
			closeSecond();
			runtime.terminateSession("owner");
			send("after terminate");
			vi.advanceTimersByTime(1);
			expect(rebound).toHaveBeenCalledTimes(1);
		} finally {
			vi.useRealTimers();
		}
	});
	it("isolates nested owners, follows rebinds, rejects late callbacks and leaves delivery independent of expiry", async () => {
		vi.useFakeTimers();
		try {
			let state: SubagentState | undefined;
			let emit: ((event: AgentSessionEvent) => void) | undefined;
			let settle: (() => void) | undefined;
			const delivered: string[] = [];
			const runtime = new Runtime({
				createRunner: (callbacks) => ({
					start: (current) => {
						state = current;
						emit = (event) => callbacks.onConversationEvent?.(current, event);
						settle = () => callbacks.onSettled(current, "done", { result: "delivered" });
						return Promise.resolve();
					},
					respond: () => {},
					abort: () => {},
				}),
			});
			const events = createEventBus();
			runtime.activateSession({
				cwd: "/",
				sessionId: "parent",
				sendMessage: () => {},
				isIdle: () => true,
				events,
			});
			const id = runtime.spawn(agentConfig, "private", "/", "parent", {
				model: undefined,
				modelRegistry,
				agentDir: "/",
				availableToolNames: [],
			});
			expect(runtime.getConversationForOwner("child", id)).toBeUndefined();
			expect(runtime.getConversationForOwner("child", "missing")).toBeUndefined();
			const snapshot = runtime.getConversationForOwner("parent", id);
			if (!snapshot || !emit || !settle || !state) throw new Error("missing capture");
			snapshot.items.pop();
			snapshot.agent.brief = "changed";
			expect(runtime.getConversationForOwner("parent", id)?.items).toHaveLength(1);
			expect(runtime.getConversationForOwner("parent", id)?.agent.brief).toBe("");
			const initial = { role: "user" as const, content: "private", timestamp: 0 };
			emit({ type: "message_start", message: initial });
			emit({ type: "message_end", message: initial });
			let changes = 0;
			const unsubscribe = runtime.subscribeConversation("parent", id, () => changes++);
			const user = { role: "user" as const, content: "extra", timestamp: 1 };
			emit({ type: "message_start", message: user });
			emit({ type: "message_end", message: user });
			vi.runOnlyPendingTimers();
			expect(changes).toBe(1);
			unsubscribe();
			runtime.terminateSession("parent");
			expect(runtime.getConversationForOwner("parent", id)).toBeDefined();
			settle();
			emit({ type: "message_start", message: { role: "user", content: "late", timestamp: 2 } });
			expect(runtime.getConversationForOwner("parent", id)?.items).toHaveLength(2);
			vi.advanceTimersByTime(30_001);
			expect(runtime.getConversationForOwner("parent", id)).toBeUndefined();
			runtime.activateSession({
				cwd: "/",
				sessionId: "parent",
				sendMessage: (message) => delivered.push(String(message.content)),
				isIdle: () => true,
				events: createEventBus(),
			});
			vi.advanceTimersByTime(1);
			expect(delivered).toEqual([expect.stringContaining("delivered")]);
		} finally {
			vi.useRealTimers();
		}
	});

	it("expires recent transcripts on the timer without a lookup", () => {
		vi.useFakeTimers();
		try {
			let settle: (() => void) | undefined;
			const runtime = new Runtime({
				createRunner: (callbacks) => ({
					start: (state) => {
						settle = () => callbacks.onSettled(state, "done", {});
						return Promise.resolve();
					},
					respond: () => {},
					abort: () => {},
				}),
			});
			runtime.activateSession({
				cwd: "/",
				sessionId: "owner",
				sendMessage: () => {},
				isIdle: () => true,
				events: createEventBus(),
			});
			const id = runtime.spawn(agentConfig, "task", "/", "owner", {
				model: undefined,
				modelRegistry,
				agentDir: "/",
				availableToolNames: [],
			});
			const changed = vi.fn();
			const unsubscribe = runtime.subscribeConversation("owner", id, changed);
			if (!settle) throw new Error("runner did not start");
			settle();
			vi.advanceTimersByTime(1);
			expect(changed).toHaveBeenCalledTimes(1);
			vi.advanceTimersByTime(30_001);
			expect(changed).toHaveBeenCalledTimes(2);
			unsubscribe();
		} finally {
			vi.useRealTimers();
		}
	});

	it("captures followups and retains waiting agents on done without delivering a second result", () => {
		let state: SubagentState | undefined;
		let settle: (() => void) | undefined;
		const deliveries: string[] = [];
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (current) => {
					state = current;
					current.session = {
						dispose: () => {},
						abort: async () => {},
						abortCompaction: () => {},
					} as unknown as AgentSession;
					settle = () => callbacks.onSettled(current, "waiting", { result: "question" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		runtime.activateSession({
			cwd: "/",
			sessionId: "owner",
			sendMessage: (message) => deliveries.push(String(message.content)),
			isIdle: () => true,
			events: createEventBus(),
		});
		const id = runtime.spawn(agentConfig, "task", "/", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/",
			availableToolNames: [],
		});
		if (!settle || !state) throw new Error("runner did not start");
		settle();
		expect(runtime.respond(id, "followup", "owner")).toEqual({});
		settle();
		expect(runtime.done(id, "owner")).toEqual({});
		expect(
			runtime
				.getConversationForOwner("owner", id)
				?.items.map((item) => (item.kind === "tool" ? item.name : item.text)),
		).toEqual(["task", "followup"]);
		expect(runtime.getConversationForOwner("owner", id)?.agent.status).toBe("done");
		expect(deliveries).toHaveLength(2);
	});

	it("keeps parent and child owned conversations mutually isolated after abort", () => {
		const runtime = new Runtime({
			createRunner: () => ({ start: () => Promise.resolve(), respond: () => {}, abort: () => {} }),
		});
		const context = { model: undefined, modelRegistry, agentDir: "/", availableToolNames: [] };
		const parentAgent = runtime.spawn(agentConfig, "parent secret", "/", "root", context);
		const childAgent = runtime.spawn(agentConfig, "child secret", "/", "child-session", context);
		expect(runtime.getConversationForOwner("root", childAgent)).toBeUndefined();
		expect(runtime.getConversationForOwner("child-session", parentAgent)).toBeUndefined();
		expect(runtime.abortOwned(childAgent, "child-session", { reason: "stop" })).toBe(true);
		expect(runtime.getConversationForOwner("root", childAgent)).toBeUndefined();
		expect(runtime.getConversationForOwner("child-session", childAgent)).toMatchObject({
			agent: { status: "aborted" },
			items: [{ text: "child secret" }],
		});
	});

	it("retains the task on bootstrap error and ignores late callbacks", async () => {
		let late: (() => void) | undefined;
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					late = () =>
						callbacks.onConversationEvent?.(state, {
							type: "message_start",
							message: { role: "user", content: "late", timestamp: 1 },
						});
					return Promise.reject(new Error("bootstrap failed"));
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const id = runtime.spawn(agentConfig, "initial task", "/", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/",
			availableToolNames: [],
		});
		await Promise.resolve();
		await Promise.resolve();
		late?.();
		expect(runtime.getConversationForOwner("owner", id)).toMatchObject({
			agent: { status: "error" },
			items: [{ kind: "user", text: "initial task" }],
		});
	});

	it("enforces the global recent byte budget independently of count", () => {
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					for (let i = 0; i < 15; i++)
						callbacks.onConversationEvent?.(state, {
							type: "tool_execution_end",
							toolCallId: `${i}`,
							toolName: "tool",
							result: { content: [{ type: "text", text: "x".repeat(16_000) }] },
							isError: false,
						});
					callbacks.onSettled(state, "done", {});
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const spawn = (owner: string) =>
			runtime.spawn(agentConfig, "task", "/", owner, {
				model: undefined,
				modelRegistry,
				agentDir: "/",
				availableToolNames: [],
			});
		const first = spawn("first");
		for (let i = 0; i < 39; i++) spawn(`owner-${i}`);
		expect(runtime.getConversationForOwner("first", first)).toBeUndefined();
		expect(runtime.getFleetSnapshotForOwner("first").recent).toEqual([]);
	});

	it("evicts summaries and conversations together at per-owner and global limits", () => {
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					callbacks.onSettled(state, "done", { result: "ok" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const spawn = (owner: string) =>
			runtime.spawn(agentConfig, "task", "/", owner, {
				model: undefined,
				modelRegistry,
				agentDir: "/",
				availableToolNames: [],
			});
		const first = spawn("owner");
		for (let i = 0; i < CONVERSATION_RECENT_PER_OWNER; i++) spawn("owner");
		expect(runtime.getConversationForOwner("owner", first)).toBeUndefined();
		expect(runtime.getFleetSnapshotForOwner("owner").recent).toHaveLength(
			CONVERSATION_RECENT_PER_OWNER,
		);
		const globalFirst = spawn("global-first");
		for (let i = 0; i < CONVERSATION_RECENT_MAX_COUNT; i++) spawn(`owner-${i}`);
		expect(runtime.getConversationForOwner("global-first", globalFirst)).toBeUndefined();
		expect(runtime.getFleetSnapshotForOwner("global-first").recent).toEqual([]);
	});
});

describe("Runtime session bindings", () => {
	it("keeps active agents and pending results across a session rebind", async () => {
		let abortCalls = 0;
		let settleAgent: (() => void) | undefined;
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					settleAgent = () => callbacks.onSettled(state, "done", { result: "completed" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {
					abortCalls++;
				},
			}),
		});
		const initialEvents = createEventBus();
		const reboundEvents = createEventBus();
		const deliveredMessages: string[] = [];
		const sessionId = "session";

		runtime.activateSession({
			cwd: "/root",
			sessionId,
			sendMessage: () => {},
			isIdle: () => true,
			events: initialEvents,
		});
		const agentId = runtime.spawn(agentConfig, "task", "/root", sessionId, {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
		});

		runtime.terminateSession(sessionId);

		expect(abortCalls).toBe(0);
		expect(runtime.getActiveSummariesForOwner(sessionId).map((agent) => agent.id)).toEqual([
			agentId,
		]);
		if (!settleAgent) throw new Error("Fake runner did not start the agent");
		settleAgent();

		runtime.activateSession({
			cwd: "/root",
			sessionId,
			sendMessage: (message) => {
				deliveredMessages.push(String(message.content));
			},
			isIdle: () => true,
			events: reboundEvents,
		});
		await new Promise((resolve) => setTimeout(resolve, 0));

		expect(deliveredMessages).toEqual([expect.stringContaining("completed")]);
	});

	it("settles a rejected runner start as an error", async () => {
		const runtime = new Runtime({
			createRunner: () => ({
				start: () => Promise.reject(new Error("bootstrap failed")),
				respond: () => {},
				abort: () => {},
			}),
		});
		const events = createEventBus();
		const changes: string[] = [];
		events.on(TEAM_RUNTIME_CHANGED_EVENT, (payload) => {
			if (isTeamRuntimeChangedPayload(payload)) changes.push(payload.reason);
		});
		runtime.activateSession({
			cwd: "/root",
			sessionId: "owner",
			sendMessage: () => {},
			isIdle: () => true,
			events,
		});
		runtime.spawn(agentConfig, "task", "/root", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
		});

		await Promise.resolve();
		await Promise.resolve();
		expect(changes).toContain("settled");
		expect(runtime.getActiveSummariesForOwner("owner")).toEqual([]);
	});

	it("emits an owner runtime change when an attached session exposes its model", async () => {
		let attach: (() => void) | undefined;
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					attach = () => {
						state.model = "provider/model-id";
						callbacks.onSessionAttached(state, "subagent-session");
					};
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const events = createEventBus();
		const changes: string[] = [];
		const sessionId = "owner";
		events.on(TEAM_RUNTIME_CHANGED_EVENT, (payload) => {
			if (isTeamRuntimeChangedPayload(payload)) changes.push(payload.reason);
		});
		runtime.activateSession({
			cwd: "/root",
			sessionId,
			sendMessage: () => {},
			isIdle: () => true,
			events,
		});
		runtime.spawn(agentConfig, "task", "/root", sessionId, {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
		});

		if (!attach) throw new Error("Fake runner did not start the agent");
		attach();
		expect(changes).toContain("model_updated");
	});

	it("reports changed-session work only as a detached count and preserves ownership controls", () => {
		const runtime = new Runtime({
			createRunner: () => ({ start: () => Promise.resolve(), respond: () => {}, abort: () => {} }),
		});
		const agentId = runtime.spawn(agentConfig, "private task", "/root", "old-session", {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
			brief: "private brief",
		});

		expect(runtime.getFleetSnapshotForOwner("new-session")).toEqual({
			active: [],
			recent: [],
			detachedActiveCount: 1,
		});
		expect(runtime.abortOwned(agentId, "new-session", { reason: "nope" })).toBe(false);
		expect(runtime.respond(agentId, "nope", "new-session").error).toContain("different session");
	});

	it("retains settled rows for thirty seconds without retaining them indefinitely", () => {
		let now = 1_000;
		let settle: (() => void) | undefined;
		const runtime = new Runtime({
			now: () => now,
			createRunner: (callbacks) => ({
				start: (state) => {
					settle = () => callbacks.onSettled(state, "done", { result: "complete" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		runtime.spawn(agentConfig, "task", "/root", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
		});
		if (!settle) throw new Error("Fake runner did not start the agent");
		settle();

		expect(runtime.getFleetSnapshotForOwner("owner").recent).toHaveLength(1);
		now += 30_001;
		expect(runtime.getFleetSnapshotForOwner("owner").recent).toEqual([]);
	});

	it("clears only terminated owner history while preserving active work and pending results", () => {
		let settle: (() => void) | undefined;
		const runtime = new Runtime({
			createRunner: (callbacks) => ({
				start: (state) => {
					settle = () => callbacks.onSettled(state, "done", { result: "after reload" });
					return Promise.resolve();
				},
				respond: () => {},
				abort: () => {},
			}),
		});
		const firstId = runtime.spawn(agentConfig, "first", "/root", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
		});
		const firstState = runtime.getActiveSummariesForOwner("owner");
		expect(firstState.map((agent) => agent.id)).toEqual([firstId]);
		if (!settle) throw new Error("Fake runner did not start the agent");
		settle();

		const snapshot = runtime.getFleetSnapshotForOwner("owner");
		snapshot.recent.pop();
		expect(runtime.getFleetSnapshotForOwner("owner").recent).toHaveLength(1);
		runtime.terminateSession("owner");
		expect(runtime.getFleetSnapshotForOwner("owner").recent).toEqual([]);

		const activeId = runtime.spawn(agentConfig, "active", "/root", "owner", {
			model: undefined,
			modelRegistry,
			agentDir: "/agents",
			availableToolNames: [],
		});
		expect(runtime.getActiveSummariesForOwner("owner").map((agent) => agent.id)).toEqual([
			activeId,
		]);
	});

	it("keeps a parent session bound while a child session starts and stops", () => {
		const runtime = new Runtime();
		const rootEvents = createEventBus();
		const childEvents = createEventBus();
		const warnings: string[] = [];
		rootEvents.on(TEAM_WARNING_EVENT, (payload) => {
			if (isTeamWarningPayload(payload)) {
				warnings.push(payload.message);
			}
		});

		runtime.activateSession({
			cwd: "/root",
			sessionId: "root",
			sendMessage: () => {},
			isIdle: () => true,
			events: rootEvents,
		});
		runtime.activateSession({
			cwd: "/child",
			sessionId: "child",
			sendMessage: () => {},
			isIdle: () => true,
			events: childEvents,
		});

		runtime.reportWarning("root", "while child is active");
		runtime.terminateSession("child");
		runtime.reportWarning("root", "after child stops");

		expect(warnings).toEqual(["while child is active", "after child stops"]);
	});
});
