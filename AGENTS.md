# AGENTS.md

This file guides coding agents working anywhere in this repository.
It is repo-wide guidance for the root dotfiles repo.

## Scope

- Apply this file for the whole repo.
- If you edit files under `configs/opencode/`, also follow `configs/opencode/AGENTS.md`.
- No repo-level Cursor rules were found in `.cursor/rules/` or `.cursorrules`.
- No repo-level Copilot rules were found in `.github/copilot-instructions.md`.

## Repo Shape

- Primary stack: Nix flakes, `nix-darwin`, and `home-manager`.
- Main entrypoint: `flake.nix`.
- User-facing setup docs: `README.md`.
- Most repo files are Nix modules under `darwin/`, `home/`, `modules/`, and `overlays/`.
- There is a smaller TypeScript/Bun subtree under `configs/opencode/`.
- This is a personal dotfiles repo; prefer small, targeted edits over broad rewrites.

## Source Of Truth

- Build/apply flow is documented in `README.md`.
- Shell aliases in `home/fish.nix` mirror common rebuild commands.
- Existing strict agent rules for the OpenCode subtree live in `configs/opencode/AGENTS.md`.
- TypeScript strictness for that subtree lives in `configs/opencode/tsconfig.json`.

## Build And Apply Commands

- Build the Darwin system: `nix build .#darwinConfigurations.g0m.system`
- Apply the built result: `./result/sw/bin/darwin-rebuild switch --flake .#g0m`
- Rebuild directly: `darwin-rebuild switch --flake .#g0m`
- Update flake inputs: `nix flake update ~/.dotfiles`
- Fish aliases you may see in docs or local shells:
- `drb` => `sudo darwin-rebuild build --flake ${nixConfigDirectory}`
- `drs` => `sudo darwin-rebuild switch --flake ${nixConfigDirectory}`
- `flakeup` => `nix flake update ${nixConfigDirectory}`
- `nb` => `nix build`

## Lint, Format, Typecheck, Test

- There is no documented repo-wide `lint` command.
- There is no documented repo-wide `test` command.
- There is no documented `flake check` workflow in this repo.
- Do not invent verification commands and present them as established repo workflow.
- If you change Nix files, prefer verifying with the documented build/rebuild flow.
- If you change only a small Nix module and full rebuild is too heavy, explain what you did not verify.
- Formatting/lint tools installed in the environment include `nixfmt`, `eslint_d`, and `stylua`.
- Those tools are available, but no repo-local config was found for root-wide automated lint runs.

## Single Test Guidance

- No repo-specific single-test command is documented at the root.
- No root test runner config was found.
- For `configs/opencode/`, no local `package.json` scripts were found either.
- If you add or discover tests later, document exact single-test invocations near the affected subtree.
- Until then, be explicit: say that no single-test workflow is defined here.

## Package Managers And Tooling

- Treat Nix as the primary package/configuration system.
- `bun` is installed and used in `configs/opencode/`, but that subtree has minimal manifest metadata.
- Do not assume npm, pnpm, yarn, or turbo are canonical for this repo just because the tools exist.
- Prefer commands already documented in `README.md` over guessed alternatives.

## Editing Principles

- Make minimal, surgical changes.
- Preserve existing structure and naming.
- Avoid opportunistic rewrites of unrelated dotfiles.
- Keep comments sparse; add them only when logic is non-obvious.
- Preserve user-specific values, machine names, usernames, paths, and emails unless the task is explicitly about them.
- Be careful with secrets and local paths.
- Never normalize the whole repo's formatting as a side effect.

## Nix Style

- Follow the prevailing module shape: argument attrset, then `let`, then returned attrset.
- Use `inherit (...)` when it improves clarity and matches nearby code.
- Use `mkIf`, `optionalString`, and related lib helpers instead of duplicating branching logic.
- Keep attrsets and lists vertically formatted like surrounding files.
- Prefer descriptive attribute names such as `nixDarwinCommonModules` and `homeManagerModules`.
- Keep platform or user conditionals explicit and local.
- When editing package lists, preserve grouping comments and ordering style.
- Avoid clever abstraction unless repetition is clearly harmful.
- Prefer extending existing modules over creating new top-level files unless needed.

## TypeScript Style (`configs/opencode/`)

- Respect `configs/opencode/AGENTS.md` when working in that subtree.
- Keep strict typing intact.
- Do not use `any`.
- Do not use non-null assertions.
- Do not use type assertions when a typed boundary parser can solve it.
- Prefer `import type` for type-only imports.
- Prefer `node:` imports for Node built-ins.
- Parse uncertain input at boundaries and return trusted typed structures.
- Prefer discriminated unions for success/error states.
- Favor guard clauses and early returns.
- Throw descriptive errors when failure should be loud.
- Keep helper names explicit and intention-revealing.
- Do not leave debug statements behind.

## Naming Conventions

- Match the surrounding file's style before introducing new names.
- In Nix, use lower camel case for local bindings and descriptive attr names.
- In TypeScript, use `PascalCase` for types/classes and `camelCase` for values/functions.
- Keep filenames aligned with existing patterns in the subtree you edit.
- Prefer names that describe domain meaning, not implementation trivia.

## Imports And Dependencies

- In TypeScript, group built-in imports first, then package imports, then local imports if you touch the file.
- In Nix, keep `inherit` and `with pkgs;` usage consistent with nearby files.
- Do not add new dependencies unless required for the task.
- If adding a dependency, place it in the narrowest correct scope.

## Error Handling

- Fail early on invalid inputs.
- Make illegal states unrepresentable where practical.
- Prefer explicit error messages over silent fallback, unless fallback is already established behavior.
- When adding fallback logic, keep it observable and narrowly scoped.
- Preserve existing operational behavior for machine-specific config unless the task says otherwise.

## Verification Expectations

- Verify with the lightest command that meaningfully covers the change.
- For repo-wide Nix changes, the best available verification is usually the documented build or rebuild path.
- For `configs/opencode/` TypeScript changes, use local typecheck/build commands only if that subtree actually defines them or the task explicitly wants ad hoc verification.
- If no reliable automated check exists, say so plainly and mention the closest available validation.

## SCM And Safety

- Check for `.jj/` before VCS commands; use `jj` if present, otherwise `git`.
- Never use destructive VCS commands unless explicitly requested.
- Do not overwrite unrelated local changes.
- Do not commit unless the user explicitly asks.
- Do not add tool or model attribution to commits or PR text.

## Good Agent Behavior Here

- Read the local file you plan to edit and one or two adjacent files first.
- Infer conventions from nearby code, not from generic templates.
- When docs and code disagree, call out the mismatch.
- When automation is missing, be honest instead of guessing.
- Leave the repo easier to maintain than you found it.
