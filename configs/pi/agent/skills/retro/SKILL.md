---
name: retro
description: "Conduct a retrospective on a coding session."
disable-model-invocation: true
---

The user has asked for a **retrospective**. You are suggesting improvements to the coding agent's **environment** to improve future runs.

## Steps

1. Call the Skill tool with `writing-for-agents` for the writing style guide.
2. Read the primary sources for the session the user specifies, defaulting to the current session. Find the main transcript and relevant subagent transcripts or logs where available; use task outputs and configuration as supporting sources. Identify the sources examined and any missing transcripts or inaccessible evidence. Do not infer what happened inside an unavailable transcript from its final output alone.
3. Examine the categories below against the evidence. Cite concrete events, tool calls, outputs, or file locations; distinguish observations from hypotheses and state uncertainty. Recommend a change only when evidence shows a recurring pattern, not a one-off issue. Report isolated observations separately without a recommendation; if sources are insufficient to establish recurrence, say so.

- **Navigation**: how easy was it for the agent to find the right files? Are there hidden dependencies between files? Would a **navigation pointer** make it easier? _Use when_ the session took a long time to find a piece of information.
- **Automated checks**: could linting, typing, tests, or filesystem checks catch a recurring error? Inspect existing repo commands and CI first; an unwired or broken check may be the issue. Weigh a deterministic check against its maintenance cost and the frequency and impact of the error; absence of a guardrail alone is not evidence of a problem.
- **Coding standards**: when implementation or review repeatedly misses an expectation, consider whether to clarify or remove an existing standard or add one. Mechanical patterns may suit an existing or proportionate automated check; judgment calls may belong in standards. Consider who needs the guidance during implementation as well as review, rather than assuming it belongs only to the reviewer.
- **Global AGENTS.md**: are there any steering instructions that should be moved to coding standards (or automated checks) instead? _Use when_ the AGENTS.md file is particularly large - in the repo OR the user's global scope.
- **Tool economy**: did the agent make expensive tool calls that could be streamlined? Is there any custom tooling (CLI's, MCP's) that is particularly token-inefficient? _Use when_ the agent made an expensive tool call.
- **No-ops**: look for instructions in steering files that don't modify the agent's behavior. _Use when_ the steering files are large and unwieldy.
- **Information access**: look for opportunities to increase the agent's access to information. Teeing dev server logs, readonly access to third-party services. _Use when_ a crucial piece of information was not available to the agent.
- **Delegation and routing**: assess whether work was delegated or handled directly at the right time, including handoffs and coordination costs. Where subagent evidence exists, assess role fit, tool access, prompts, and outcomes. Inspect Pi team system prompts when the evidence points to their influence; distinguish prompt effects from other possible causes.

## Instruction placement

Match proposed guidance to its audience and point of use: put broadly applicable steering and navigation in the relevant global or repo `AGENTS.md`; durable project facts and workflows in docs; task-triggered procedures in skills; and shared implementation or review expectations in coding standards. Consider whether implementers, reviewers, or both need an instruction, and link to a single source rather than duplicating it. Prefer proportionate existing mechanical checks where they reliably enforce a recurring concern.

4. Present evidence-backed recommendations in severity order, with the recurring examples and intended placement or check. Separately report isolated observations, hypotheses, and source limitations; make clear when there is insufficient evidence for a recommendation.

## Reference

### Repo Files

If the improvements are only relevant and affect a single repo. You have access to several files in the repo:

- `AGENTS.md`: these files are pushed to the context window of any agent working in this repo. They should be used incredibly sparingly, usually only for **navigation pointers** to other files.
- `CODING_STANDARDS.md`: this file is read during review, not implementation. Add **navigation pointers** to docs folders if the standards file gets more than 1,000 lines long.
- Docs: use docs as references files, pointed to by other files. Look for existing docs before writing new ones.

### Global Files

If the improvements comes from issues that are affecting multiple projects, might make sense to improve the harness itself.

- Skills: use skills for docs (since their description goes into the agent's context window), or for user-invoked commands. Follow the advice in the `build-skill` skill:
  - For Pi related skills use `~/.pi/agent/skills`;
  - For agent tool independent skills use the `~/.agents/skills`.
- Agents: for specific agent workflows/improvements check `~/.pi/agent/agents`.
- Extensions: for errors/enhancements that you detected as being caused/belong to a Pi extension, check `~/.pi/agent/extensions`.
