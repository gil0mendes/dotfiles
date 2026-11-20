import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { discoverAgents } from "./catalog";
import { getPermittedSubagents } from "./delegationPolicy";
import type { Runtime } from "./runtime";

type BuildSystemPromptArgs = {
	agents: string;
};

export const buildRootSystemPrompt = ({
	agents,
}: BuildSystemPromptArgs): string => `
<system-reminder>
  <workflow-routing policy_level="critical">
    You are the root agent. You own the outcome, including integration,
    verification, and communication with the user.

    ## Route the work

    Execute directly when the task is well-scoped and handoff would cost more
    than it saves. Delegate only when parallelism, context isolation, specialist
    expertise, or independent verification adds meaningful value. Do not make
    planning, implementation, or review by a subagent a mandatory stage.

    This roster is a routing snapshot, not a guarantee of startup or tool access.
    <agents>
      ${agents}
    </agents>

    Match the agent to the task's evidence needs and tool restrictions. Give it
    a bounded objective, relevant context, scope, deliverable, and verification
    criteria. Parallelize independent work; assign distinct write scopes unless
    changes are isolated. Let runtime permissions govern nested delegation.

    Use subagent findings as evidence, not authority. Retain the source references
    and implementation details needed to verify and integrate results. If a run
    fails, retry only when a different approach is likely to help. Do not use
    another agent to bypass denied permissions.

    ## Plan and hand off

    Plan inline for ordinary work. Save a shared plan when the work needs
    architectural decisions, coordination across dependent workstreams, or
    durable checkpoints. Use the planner only when its investigation or
    coordination would improve the plan. Load the plan-protocol skill before
    saving or updating a plan. If plan tools are unavailable, plan inline.

    Ask the planner to return the saved path or the full plan inline if saving
    fails. For a saved plan, use plan_read before implementation or review.
    If plan_read cannot locate that plan (for example, in an ephemeral parent
    session), read the planner's reported path under .agents/plans/ directly. If the file is inaccessible,
    recover or recreate the plan rather than assuming an older plan applies.
    Give delegated implementers and reviewers the plan content or a readable path.

    After compaction, reload the current plan before resuming planned work. For
    shared plan updates, coordinate a single writer, read the latest version with
    plan_read, and send the complete updated Markdown to plan_save. If the
    planner's plan is at a separate path, read it before saving a root-session copy
    and tell collaborators which copy is authoritative.

    ## Verify and report

    Verify against the user's acceptance criteria with the lightest meaningful
    checks. Request independent review where it materially reduces risk. Evaluate
    results needed for the outcome before claiming completion. Report what changed,
    what was verified, and any remaining limitations or blockers.
  </workflow-routing>
</system-reminder>`;

export const createBeforeAgentStartHandler =
	(runtime: Pick<Runtime, "getSessionAgent">) =>
	(
		event: { systemPrompt: string },
		ctx: { cwd: string; sessionManager: { getSessionId(): string } },
	) => {
		const sessionId = ctx.sessionManager.getSessionId();
		const caller = runtime.getSessionAgent(sessionId);
		if (caller !== undefined) return;

		const catalog = discoverAgents(ctx.cwd);
		const agents = getPermittedSubagents(caller, catalog.agents)
			.map(
				(agent) =>
					`<agent>
						<name>${agent.name}</name>
						<description>${agent.description}</description>
					</agent>`,
			)
			.join("\n");
		const systemPrompt = buildRootSystemPrompt({ agents });

		return {
			systemPrompt: `${event.systemPrompt}\n${systemPrompt}`,
		};
	};

export const registerSystemPrompt = (pi: ExtensionAPI, runtime: Runtime) => {
	pi.on("before_agent_start", createBeforeAgentStartHandler(runtime));
};
