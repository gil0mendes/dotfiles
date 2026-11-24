import { describe, expect, it, vi } from "vitest";
import type { AgentSession, EventBus, ModelRegistry } from "@earendil-works/pi-coding-agent";
import { Runtime } from "./runtime";
import type { SubagentState, SettledSubagentStatus } from "./types";
import { SubagentSessionRunner, type SubagentRunnerCallbacks } from "./subagentRunner";

const sessionIdFor = (agentId: string) => `session:${agentId}`;

function setup(prompt?: (message: string) => Promise<void>) {
	const states = new Map<string, SubagentState>();
	const dispose = vi.fn();
	const respond = vi.fn();
	const abort = vi.fn();
	const sendMessage = vi.fn();
	let runnerCallbacks: SubagentRunnerCallbacks | undefined;
	const runtime = new Runtime({
		createRunner: (callbacks) => {
			runnerCallbacks = callbacks;
			const runner = new SubagentSessionRunner(callbacks);
			if (prompt) respond.mockImplementation((state, message) => runner.respond(state, message));
			return {
				async start(state) {
					state.session = {
						prompt,
						messages: [],
						dispose: () => dispose(state.id),
					} as unknown as AgentSession;
					states.set(state.id, state);
					callbacks.onSessionAttached(state, sessionIdFor(state.id));
				},
				respond,
				abort,
			};
		},
	});
	runtime.activateSession({
		sessionId: "root",
		cwd: "/",
		isIdle: () => true,
		sendMessage,
		events: { emit: vi.fn() } as unknown as EventBus,
	});
	const spawn = (name: string, owner = "root") =>
		runtime.spawn(
			{
				name,
				type: "agent",
				description: name,
				systemPrompt: "",
				filePath: `/agents/${name}.md`,
			},
			name,
			"/",
			owner,
			{
				model: undefined,
				modelRegistry: {} as ModelRegistry,
				agentDir: "/",
				availableToolNames: [],
			},
		);
	const finish = (id: string, status: SettledSubagentStatus = "done") => {
		const state = states.get(id);
		if (!state || !runnerCallbacks) throw new Error(`No fake run ${id}`);
		runnerCallbacks.onSettled(
			state,
			status,
			status === "error" ? { error: state.task } : { result: state.task },
		);
	};
	return { runtime, spawn, finish, respond, abort, sendMessage, dispose };
}

describe("nested team runs", () => {
	it.each([false, true])("joins both child results exactly once (reverse order: %s)", (reverse) => {
		const { runtime, spawn, finish, respond, sendMessage, dispose } = setup();
		const parent = spawn("planner");
		const first = spawn("first report", sessionIdFor(parent));
		const second = spawn("second report", sessionIdFor(parent));
		finish(reverse ? second : first);
		finish(parent);
		expect(runtime.getActiveSummariesForOwner("root")[0]?.status).toBe("waiting-for-children");
		expect(sendMessage).not.toHaveBeenCalled();
		expect(dispose).not.toHaveBeenCalledWith(parent);
		expect(respond).not.toHaveBeenCalled();
		finish(reverse ? first : second, "error");
		expect(respond).toHaveBeenCalledExactlyOnceWith(
			expect.objectContaining({ id: parent }),
			expect.stringMatching(/(?=.*first report)(?=.*second report)/s),
		);
		const wave = spawn("more research", sessionIdFor(parent));
		finish(wave);
		finish(parent);
		expect(respond).toHaveBeenCalledTimes(2);
		expect(sendMessage).not.toHaveBeenCalled();
		finish(parent);
		finish(parent);
		expect(sendMessage).toHaveBeenCalledOnce();
	});

	it("wakes the parent for interactive input and joins explicit closure", () => {
		const { runtime, spawn, finish, respond, sendMessage } = setup();
		const parent = spawn("planner");
		const question = spawn("question", sessionIdFor(parent));
		const other = spawn("other", sessionIdFor(parent));
		finish(parent);
		finish(question, "waiting");
		expect(respond).toHaveBeenCalledOnce();
		expect(runtime.done(question, sessionIdFor(parent))).toEqual({});
		finish(parent);
		expect(respond).toHaveBeenCalledOnce();
		finish(other, "aborted");
		expect(respond).toHaveBeenCalledTimes(2);
		finish(parent);
		expect(sendMessage).toHaveBeenCalledOnce();
	});

	it("does not start a second continuation while a real runner prompt is active", async () => {
		const firstPrompt = Promise.withResolvers<void>();
		const secondPrompt = Promise.withResolvers<void>();
		const prompt = vi
			.fn()
			.mockReturnValueOnce(firstPrompt.promise)
			.mockReturnValueOnce(secondPrompt.promise);
		const { runtime, spawn, finish, sendMessage } = setup(prompt);
		const parent = spawn("planner");
		const question = spawn("question", sessionIdFor(parent));
		const other = spawn("other", sessionIdFor(parent));
		finish(parent);
		finish(question, "waiting");
		finish(other);
		expect(prompt).toHaveBeenCalledOnce();
		expect(runtime.done(question, sessionIdFor(parent))).toEqual({});
		expect(prompt).toHaveBeenCalledOnce();
		firstPrompt.resolve();
		await firstPrompt.promise;
		expect(prompt).toHaveBeenCalledTimes(2);
		expect(sendMessage).not.toHaveBeenCalled();
		secondPrompt.resolve();
		await secondPrompt.promise;
		expect(sendMessage).toHaveBeenCalledOnce();
	});

	it("lets the owner respond when a child still needs input after the continuation", () => {
		const { runtime, spawn, finish, respond, sendMessage } = setup();
		const parent = spawn("planner");
		const child = spawn("question", sessionIdFor(parent));
		finish(parent);
		finish(child, "waiting");
		finish(parent);
		expect(runtime.getActiveSummariesForOwner("root")[0]?.status).toBe("waiting");
		expect(sendMessage).toHaveBeenCalledWith(
			expect.objectContaining({
				content: expect.stringContaining("Child agents are waiting for input"),
			}),
			expect.anything(),
		);
		expect(runtime.respond(parent, "answer", "root")).toEqual({});
		expect(respond).toHaveBeenLastCalledWith(expect.objectContaining({ id: parent }), "answer");
	});

	it("cleans up a terminal branch even if the owner's result delivery throws", () => {
		const { runtime, spawn, finish, dispose, sendMessage } = setup();
		const parent = spawn("planner");
		const child = spawn("child", sessionIdFor(parent));
		finish(parent);
		sendMessage.mockImplementationOnce(() => {
			throw new Error("delivery failed");
		});
		expect(() => runtime.abortOwned(parent, "root", { reason: "cancelled" })).toThrow(
			"delivery failed",
		);
		expect(dispose.mock.calls.flat()).toEqual([child, parent]);
		expect(runtime.getActiveSummariesForOwner(sessionIdFor(parent))).toEqual([]);
	});

	it("retains owned agents across a session switch but cancels them on quit", () => {
		const { runtime, spawn, finish, respond, abort } = setup();
		const parent = spawn("planner");
		const child = spawn("child", sessionIdFor(parent));
		finish(parent);
		runtime.terminateSession("root");
		expect(abort).not.toHaveBeenCalled();
		runtime.terminateSession("root", "quit");
		finish(child);
		expect(abort.mock.calls.map(([state]) => state.id)).toEqual([parent, child]);
		expect(respond).not.toHaveBeenCalled();
	});

	it("cancels descendants without waking a closed parent or touching another owner", () => {
		const { runtime, spawn, finish, respond, abort, sendMessage } = setup();
		const parent = spawn("planner");
		const child = spawn("child", sessionIdFor(parent));
		const grandchild = spawn("grandchild", sessionIdFor(child));
		const unrelated = spawn("unrelated", "other-root");
		finish(child);
		finish(parent);
		expect(runtime.abortOwned(parent, "root", { reason: "cancelled" })).toBe(true);
		finish(child);
		finish(grandchild);
		expect(abort.mock.calls.map(([state]) => state.id)).toEqual([parent, child, grandchild]);
		expect(runtime.getActiveSummariesForOwner("other-root")[0]?.id).toBe(unrelated);
		expect(respond).not.toHaveBeenCalled();
		expect(sendMessage).toHaveBeenCalledOnce();
	});
});
