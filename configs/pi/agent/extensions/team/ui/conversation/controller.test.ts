import { createEventBus, initTheme, type Theme } from "@earendil-works/pi-coding-agent";
import {
	KeybindingsManager,
	TUI_KEYBINDINGS,
	type Component,
	type TUI,
} from "@earendil-works/pi-tui";
import { beforeAll, describe, expect, it, vi } from "vitest";
import type { ConversationSnapshot } from "../../conversation";
import { Runtime, type ActiveAgentSummary } from "../../runtime";
import { createFleetController } from "../controller";

type Context = Parameters<ReturnType<typeof createFleetController>["show"]>[0];
const agent = (id: string): ActiveAgentSummary => ({
	id,
	agentName: `agent-${id}`,
	brief: "brief",
	status: "running",
	turns: 0,
	contextTokens: 0,
	startedAt: 0,
	model: undefined,
	result: undefined,
	error: undefined,
});

function harness() {
	const events = createEventBus();
	const runtime = new Runtime();
	const agents = Array.from({ length: 10 }, (_, index) => agent(String(index)));
	const transcript: ConversationSnapshot = {
		agent: agent("9"),
		revision: 1,
		items: [{ kind: "user", id: "m", text: "private content" }],
		truncated: false,
	};
	vi.spyOn(runtime, "getFleetSnapshotForOwner").mockReturnValue({
		active: agents,
		recent: [],
		detachedActiveCount: 0,
	});
	let available = true;
	const get = vi
		.spyOn(runtime, "getConversationForOwner")
		.mockImplementation((owner, id) =>
			available && owner === "owner" && id === "9" ? transcript : undefined,
		);
	const unsubscribe = vi.fn();
	let listener: (() => void) | undefined;
	const subscribe = vi
		.spyOn(runtime, "subscribeConversation")
		.mockImplementation((_owner, _id, callback) => {
			listener = callback;
			return unsubscribe;
		});
	const controller = createFleetController(events, runtime);
	let owner = "owner";
	let component: Component | undefined;
	let mounts = 0;
	const keys = new KeybindingsManager(TUI_KEYBINDINGS, {});
	const tui = { requestRender: () => {}, terminal: { rows: 40 } } as unknown as TUI;
	const custom: Context["ui"]["custom"] = (factory) =>
		new Promise((resolve) => {
			mounts++;
			const result = factory(
				tui,
				{} as Theme,
				keys as unknown as Parameters<Parameters<Context["ui"]["custom"]>[0]>[2],
				resolve,
			);
			if (result instanceof Promise) throw new Error("unexpected async component");
			component = result;
		});
	const ctx: Context = {
		sessionManager: { getSessionId: () => owner },
		ui: { custom, notify: () => {}, input: async () => undefined, confirm: async () => false },
	};
	const current = () => {
		if (!component) throw new Error("component not mounted");
		return component;
	};
	return {
		controller,
		runtime,
		agents,
		ctx,
		current,
		get,
		subscribe,
		unsubscribe,
		transcript,
		notifyConversation: () => listener?.(),
		expire: () => {
			available = false;
		},
		mounts: () => mounts,
		setOwner: (value: string) => {
			owner = value;
		},
	};
}

const tick = async () => {
	await Promise.resolve();
	await Promise.resolve();
};

beforeAll(() => initTheme("dark", false));

describe("conversation controller lifecycle", () => {
	it("returns to the selected fleet row and window after viewing", async () => {
		const h = harness();
		const open = h.controller.show(h.ctx);
		for (let i = 0; i < 9; i++) h.current().handleInput?.("j");
		h.current().handleInput?.("v");
		await tick();
		expect(h.subscribe).toHaveBeenCalledWith("owner", "9", expect.any(Function));
		expect(h.current().render(80).join("\n")).toContain("private content");
		expect(h.get).toHaveBeenCalledWith("owner", "9");
		expect(h.subscribe.mock.invocationCallOrder[0]).toBeLessThan(
			h.get.mock.invocationCallOrder[0] ?? 0,
		);
		h.current().handleInput?.("q");
		await tick();
		expect(h.unsubscribe).toHaveBeenCalledTimes(1);
		expect(h.current().render(120).join("\n")).toContain("> agent-9");
		h.current().handleInput?.("q");
		await open;
	});
	it("does not render a stale roster after owner changes", async () => {
		const h = harness();
		const open = h.controller.show(h.ctx);
		expect(h.current().render(80).join("\n")).toContain("agent-0");
		h.setOwner("other");
		expect(h.current().render(80)).toEqual([]);
		h.controller.dispose();
		await open;
	});
	it("updates on conversation events and displays expired snapshots", async () => {
		const h = harness();
		const open = h.controller.show(h.ctx);
		for (let i = 0; i < 9; i++) h.current().handleInput?.("j");
		h.current().handleInput?.("v");
		await tick();
		h.transcript.items.push({ kind: "assistant", id: "new", text: "streamed" });
		h.transcript.revision++;
		h.notifyConversation();
		expect(h.current().render(80).join("\n")).toContain("streamed");
		h.expire();
		h.notifyConversation();
		expect(h.current().render(80).join("\n")).toContain("unavailable or expired");
		h.controller.dispose();
		await open;
	});
	it("handles completion, expiry and reopening without stale transcript data", async () => {
		const h = harness();
		const open = h.controller.show(h.ctx);
		for (let i = 0; i < 9; i++) h.current().handleInput?.("j");
		h.current().handleInput?.("v");
		await tick();
		h.transcript.agent.status = "done";
		h.transcript.expiresAt = Date.now() + 1000;
		h.notifyConversation();
		expect(h.current().render(80).join("\n")).toContain("done");
		h.expire();
		h.notifyConversation();
		expect(h.current().render(80).join("\n")).not.toContain("private content");
		h.current().handleInput?.("q");
		await tick();
		h.current().handleInput?.("v");
		await tick();
		expect(h.current().render(80).join("\n")).toContain("unavailable or expired");
		expect(h.current().render(80).join("\n")).not.toContain("private content");
		h.controller.dispose();
		await open;
	});
	it("refreshes at recent expiry without polling", async () => {
		vi.useFakeTimers();
		try {
			const h = harness();
			h.transcript.expiresAt = Date.now() + 100;
			const open = h.controller.show(h.ctx);
			const concurrent = h.controller.show(h.ctx);
			expect(h.mounts()).toBe(1);
			for (let i = 0; i < 9; i++) h.current().handleInput?.("j");
			h.current().handleInput?.("v");
			await tick();
			h.current().render(80);
			h.expire();
			const before = h.get.mock.calls.length;
			vi.advanceTimersByTime(101);
			expect(h.get.mock.calls.length).toBe(before);
			expect(h.current().render(80).join("\n")).toContain("unavailable or expired");
			expect(h.get.mock.calls.length).toBe(before + 1);
			h.controller.dispose();
			await Promise.all([open, concurrent]);
			expect(vi.getTimerCount()).toBe(0);
		} finally {
			vi.useRealTimers();
		}
	});
	it.each([
		["respond", false],
		["respond", true],
		["abort", false],
		["abort", true],
	] as const)(
		"does not act or reopen after shutdown during %s dialog (reject=%s)",
		async (action, rejectOnAbort) => {
			const h = harness();
			const selected = h.agents[0];
			if (!selected) throw new Error("missing agent");
			selected.status = action === "respond" ? "waiting" : "running";
			vi.spyOn(h.runtime, "getActiveSummariesForOwner").mockReturnValue([selected]);
			const respond = vi.spyOn(h.runtime, "respond");
			const abort = vi.spyOn(h.runtime, "abortOwned");
			let signal: AbortSignal | undefined;
			let finish: (() => void) | undefined;
			if (action === "respond") {
				h.ctx.ui.input = (_title, _placeholder, options) =>
					new Promise((resolve, reject) => {
						signal = options?.signal;
						finish = () =>
							rejectOnAbort ? reject(new Error("aborted")) : resolve("late response");
					});
			} else {
				h.ctx.ui.confirm = (_title, _message, options) =>
					new Promise((resolve, reject) => {
						signal = options?.signal;
						finish = () => (rejectOnAbort ? reject(new Error("aborted")) : resolve(true));
					});
			}
			const open = h.controller.show(h.ctx);
			h.current().handleInput?.(action === "respond" ? "r" : "a");
			await tick();
			expect(signal?.aborted).toBe(false);
			h.controller.dispose();
			expect(signal?.aborted).toBe(true);
			finish?.();
			await open;
			expect(respond).not.toHaveBeenCalled();
			expect(abort).not.toHaveBeenCalled();
			expect(h.mounts()).toBe(1);
			h.setOwner("other");
			const reopened = h.controller.show(h.ctx);
			expect(h.mounts()).toBe(2);
			h.current().handleInput?.("q");
			await reopened;
		},
	);
	it("stops rendering after owner changes and disposes without reopening", async () => {
		const h = harness();
		const open = h.controller.show(h.ctx);
		for (let i = 0; i < 9; i++) h.current().handleInput?.("j");
		h.current().handleInput?.("v");
		await tick();
		h.setOwner("other");
		expect(h.current().render(80).join("\n")).not.toContain("private content");
		h.controller.dispose();
		await open;
		expect(h.unsubscribe).toHaveBeenCalledTimes(1);
		expect(h.mounts()).toBe(2);
	});
});
