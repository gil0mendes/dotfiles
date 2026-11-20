import { describe, expect, it, vi } from "vitest";
import { discoverAgents } from "./catalog";
import { buildRootSystemPrompt, createBeforeAgentStartHandler } from "./systemPrompt";
import type { AgentConfig } from "./loader/types";

vi.mock("./catalog", () => ({
	discoverAgents: vi.fn(() => ({
		agents: [{ name: "worker", description: "Completes a bounded task" }],
	})),
}));

const subagent: AgentConfig = {
	name: "worker",
	description: "Completes a bounded task",
	type: "agent",
	systemPrompt: "Complete the task.",
	filePath: "/tmp/worker.md",
};

const createHandler = () =>
	createBeforeAgentStartHandler({
		getSessionAgent: (sessionId) => (sessionId === "subagent-session" ? subagent : undefined),
	});

const contextFor = (sessionId: string) => ({
	cwd: "/tmp",
	sessionManager: { getSessionId: () => sessionId },
});

const event = { systemPrompt: "base prompt" };

describe("root team system prompt", () => {
	it("preserves the base prompt and injects the discovered routing roster for root", () => {
		const result = createHandler()(event, contextFor("root-session"));

		expect(discoverAgents).toHaveBeenCalledWith("/tmp");
		expect(result?.systemPrompt).toMatch(/^base prompt\n/);
		expect(result?.systemPrompt).toMatch(
			/<agents>\s*<agent>\s*<name>worker<\/name>\s*<description>Completes a bounded task<\/description>\s*<\/agent>\s*<\/agents>/,
		);
	});

	it("does not inject root routing into subagent sessions", () => {
		vi.mocked(discoverAgents).mockClear();

		expect(createHandler()(event, contextFor("subagent-session"))).toBeUndefined();
		expect(discoverAgents).not.toHaveBeenCalled();
	});

	it("covers shared plan handoff, ephemeral fallback, and coordinated updates", () => {
		const prompt = buildRootSystemPrompt({ agents: "" });

		expect(prompt).toMatch(/plan_read before implementation or review/);
		expect(prompt).toMatch(/ephemeral parent\s+session\), read the planner's reported path/);
		expect(prompt).toMatch(/plan tools are unavailable, plan inline/);
		expect(prompt).toMatch(/After compaction, reload the current plan/);
		expect(prompt).toMatch(/single writer[\s\S]*?plan_read[\s\S]*?plan_save/);
	});
});
