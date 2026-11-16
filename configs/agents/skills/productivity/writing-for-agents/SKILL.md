---
name: writing-for-agents
description: Write and review instructions for agents. Use when creating or editing skills, AGENTS.md, or documents linked from agent instructions.
---

# Writing for Agents

Write for a repeatable process, not a fixed output. Apply these principles to skills, agent instruction files, and documents reached through their pointers.

For skill structure, frontmatter, progressive disclosure, and validation, load `build-skill` and follow its task-specific references. Treat invocation controls as platform-specific: check the target harness's documented behavior before relying on them.

## Context pointers and loads

A **context pointer** names material outside the current context and says when to reach it. Skill descriptions and links in agent instructions are both pointers. Their wording determines whether the material is reached.

- Lead with the concept that should trigger the pointer; state what the material contains and each distinct branch that needs it.
- Collapse synonyms for the same branch and remove identity already supplied by the target.
- Sharpen a weak pointer before inlining its target. Keep always-loaded pointers especially concise.
- Balance **context load** (material occupying the agent's window) against **cognitive load** (the human remembering what exists and when to use it). Spend human attention where judgment matters, not on avoidable lookup.

## Information hierarchy

Place each piece according to when it is needed:

1. **In-file steps:** actions in execution order.
2. **In-file reference:** definitions, rules, and facts consulted as needed; a flat set of peer rules can belong together.
3. **Disclosed reference:** branch-specific material in another file, reached through a clear pointer.

Inline what every branch needs; disclose material needed only by some branches. Keep each concept's definition, rules, and caveats together under one heading. When a long document thins attention or buries steps, split by branch or sequence rather than scattering related guidance.

## Steps and completion criteria

Give each step a checkable completion criterion. Make its demand exhaustive enough to drive the necessary legwork: “account for every modified model” is stronger than “produce a change list.” Apply the same exhaustiveness to flat reference, such as reviewing every applicable rule.

A vague criterion can invite premature completion when later steps are visible. Sharpen the criterion first. If it remains irreducibly fuzzy and rushing is observed, consider splitting the sequence across a real context boundary; an inline call does not hide later steps already in context.

## Splitting and invocation

Split only when the reduction in load or improved execution earns another document:

- **By branch:** disclose material only some paths need.
- **By sequence:** isolate a step when visible later steps demonstrably cause premature completion and a real context boundary is available.
- **By invocation:** give a skill independent discovery when its distinct trigger or reuse warrants a separate entry. Otherwise keep shared guidance in an appropriate reference and point to it.

Choose discovery and invocation options according to the target harness, including whether skills can invoke one another and how descriptions are exposed. A router can help people find a collection of manually invoked skills; do not assume it can invoke them without checking the harness.

## Leading words and positive instructions

A **leading word** is a compact, recognizable concept used consistently as a thinking and discovery hook. Prefer a familiar term with useful existing associations; define a coined term when needed. Repeating the word can anchor a behavior without repeating its full definition. Use it in pointers when it matches language people actually use.

Replace restated lists with a leading word only when the word conveys the intended distinction. Test whether it changes behavior; a vague word that merely restates the default is a no-op.

State the desired action positively. Reserve prohibitions for necessary hard guardrails and pair them with the action to take instead.

## Pruning

- Keep each meaning in one authoritative place; link to it rather than duplicating it. Repeat a leading word, not its definition.
- Treat configuration, directory layout, scripts, and command help as sources of truth. Cache hard-to-discover conventions, rationale, and gotchas, not cheap lookups that can go stale.
- Check every line for relevance to the task and current behavior. Remove stale sediment and disclose branch-specific material.
- Test each instruction against the model's default behavior. Delete a sentence that changes nothing; resolve disagreement by running the document rather than debating assumptions.
