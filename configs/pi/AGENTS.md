# AGENTS.md

Guidance for coding agents working in `configs/pi/`.

## Scope

- Applies to everything under `configs/pi/`.
- Also follow the repo-wide `/AGENTS.md`.
- This subtree contains Pi agent configuration, extensions, skills, themes, and local tests.

## Workflow

- Be concise.
- Make minimal, surgical changes.
- Read the files you plan to edit before editing.
- Preserve user-specific paths and settings unless explicitly asked to change them.
- Do not commit unless explicitly asked.
- This repo uses `jj`; check status with `jj status` before committing or describing changes.

## Pi Extension Structure

- Pi extensions live under `agent/extensions/`.
- Top-level files like `agent/extensions/answer.ts` are loaded as extensions.
- Directory extensions use `agent/extensions/<name>/index.ts`.
- Shared extension code belongs in `agent/extensions/_shared/`; do not add an `index.ts` there unless you intend Pi to load it as an extension.
- Keep reusable helpers side-effect free. Extension registration belongs in extension entrypoints.
- Damage Control code lives in `agent/extensions/damage-control/`.

## TypeScript Style

- Keep strict typing intact.
- Do not use `any`; prefer `unknown` plus boundary parsing/guards.
- Avoid non-null assertions and broad type assertions.
- Prefer `import type` for type-only imports.
- Prefer `node:` imports for Node built-ins.
- Keep helper names intention-revealing.
- Do not leave debug statements behind.

## Config Loading

- Reuse `agent/extensions/_shared/config.ts` for YAML-backed extension configs.
- Keep config parsing typed by passing a parser from `RawConfig` to the expected config type.
- Extension-specific defaults and normalization should remain next to that extension.

## Tests And Verification

- Use the lightest meaningful verification.
- For focused tests, run targeted Vitest files, for example:
  - `bunx vitest run agent/extensions/_shared/config.test.ts`
  - `bunx vitest run agent/extensions/damage-control/commands/dangerous.test.ts`
- For focused TypeScript checks, prefer targeted `tsc --noEmit` commands over full repo checks when unrelated files fail.
- If you skip verification because the user requested it, say so plainly.
