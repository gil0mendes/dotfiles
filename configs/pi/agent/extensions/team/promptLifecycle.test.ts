import type { AgentSession, PromptOptions } from "@earendil-works/pi-coding-agent";
import { expect, it, vi } from "vitest";
import { SubagentSessionRunner } from "./subagentRunner";
import type { SubagentState } from "./types";

it("does not start a resumed run after cancellation during prompt preflight", async () => {
	const preflight = Promise.withResolvers<void>();
	const finished = Promise.withResolvers<void>();
	const startRun = vi.fn();
	const onSettled = vi.fn();
	let current = true;
	const state: SubagentState = {
		id: "planner-agent",
		ownerSessionId: "root-session",
		agentConfig: {
			name: "planner",
			type: "agent",
			description: "",
			systemPrompt: "",
			filePath: "/planner.md",
		},
		status: "running",
		task: "plan",
		brief: "plan",
		startedAt: 0,
		turns: 0,
		contextTokens: 0,
		session: {
			abortCompaction: vi.fn(),
			abort: vi.fn().mockResolvedValue(undefined),
			async prompt(_text: string, options?: PromptOptions) {
				await preflight.promise;
				try {
					options?.preflightResult?.(true);
					startRun();
				} finally {
					finished.resolve();
				}
			},
		} as unknown as AgentSession,
	};
	const runner = new SubagentSessionRunner({
		isCurrent: () => current,
		onProgress: vi.fn(),
		onSessionAttached: vi.fn(),
		onSettled,
	});
	runner.respond(state, "child results");
	runner.abort(state);
	current = false;
	preflight.resolve();
	await finished.promise;
	await Promise.resolve();
	expect(startRun).not.toHaveBeenCalled();
	expect(onSettled).not.toHaveBeenCalled();
});
