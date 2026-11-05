import { describe, expect, it } from "vitest";
import { buildRootSystemPrompt, createBeforeAgentStartHandler } from "./systemPrompt";
import type { AgentConfig } from "./loader/types";

const subagent: AgentConfig = {
	name: "worker",
	description: "Completes a bounded task",
	type: "agent",
	systemPrompt: "Complete the task.",
	filePath: "/tmp/worker.md",
};

describe("root team system prompt", () => {
	it("injects routing guidance only through the root before-agent-start hook", () => {
		const handler = createBeforeAgentStartHandler({
			getSessionAgent: (sessionId) => (sessionId === "subagent-session" ? subagent : undefined),
		});
		const context = {
			cwd: "/tmp",
			sessionManager: { getSessionId: () => "root-session" },
		};

		expect(handler({ systemPrompt: "base prompt" }, context)?.systemPrompt).toContain(
			"You are the root orchestrator",
		);
		expect(
			handler(
				{ systemPrompt: "base prompt" },
				{ ...context, sessionManager: { getSessionId: () => "subagent-session" } },
			),
		).toBeUndefined();
	});

	it("defines the roster as a direct-spawn routing snapshot", () => {
		const prompt = buildRootSystemPrompt({ agents: "<agent><name>worker</name></agent>" });

		expect(prompt).toContain(
			"You are the root orchestrator coordinating multiple agents for complex work.",
		);
		expect(prompt).toContain(
			"This injected roster is a routing snapshot. You may call team_spawn directly with the name of any listed permitted agent; team_list is not required first.",
		);
		expect(prompt).toContain(
			"team_spawn reloads configuration and enforces agent existence and delegation permission before asynchronous runner startup. During startup, model and tool resolution may fall back and emit warnings.",
		);
		expect(prompt).toContain(
			"Use team_list only when you need discovery, declared capability detail, or your caller-owned active status and duplicate-work check. Its snapshot does not guarantee availability or freshness.",
		);
		expect(prompt).not.toMatch(/must call team_list before.*team_spawn/i);
		expect(prompt).not.toMatch(/team_list.*(?:must|required).*before.*team_spawn/i);
	});
});
