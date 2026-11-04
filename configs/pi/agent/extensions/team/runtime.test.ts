import { AuthStorage, createEventBus, ModelRegistry } from "@earendil-works/pi-coding-agent";
import { describe, expect, it } from "vitest";
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

const modelRegistry = ModelRegistry.inMemory(AuthStorage.inMemory());

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
