# Damage Control Extension Improvements

## Major

1. Symlink-aware path policy matching

`rules.ts` matches path rules against lexical paths only. `paths.ts` realpaths outside-cwd checks, but protected path rules can miss targets accessed through symlinks.

Impact: `zeroAccessPaths`, `readOnlyPaths`, and `noDeletePaths` can be bypassed when a cwd path points outside the workspace.

Fix:

- Resolve existing targets with `fs.realpathSync.native`.
- Match both lexical absolute path and realpath.
- Preserve current behavior for non-existent paths.

2. Strongly type `multi_tool_use.parallel` parsing

`index.ts` uses `(event.input as any).tool_uses`.

Impact:

- Violates repo type-safety rules.
- Malformed tool payloads may be handled inconsistently.
- Makes future changes fragile.

Fix:

- Add a small parser/guard for parallel tool calls.
- Return a typed array of `{ name, parameters }`.
- Avoid `any`, non-null assertions, and unsafe casts.

3. Improve container root mount detection

`commands/dangerous.ts` misses common Docker/Podman forms:

- `docker run -v /:/host alpine`
- `docker run --volume /:/host alpine`
- `docker run --mount type=bind,source=/,target=/host alpine`

Impact: dangerous container escapes may not be blocked.

Fix:

- Parse flag/value pairs.
- Detect combined and separated `-v`, `--volume`, and `--mount` values.
- Add tests for Docker and Podman variants.

## Minor

4. Extract shared outside-cwd prompt flow

`index.ts` duplicates path-confirmation behavior between `tool_call` and `user_bash`.

Impact:

- Behavior drift.
- Inconsistent logging/status updates.
- Harder to test.

Fix:

- Extract shared helper for checking allowed paths.
- Extract shared helper for prompting UI.
- Extract shared helper for applying session/always grants.
- Extract shared helper for emitting events and appending logs.

5. Fix YAML single-quote persistence reload

`paths.ts` persists single quotes by doubling apostrophes, but `config.ts` `stripQuotes` does not unescape doubled apostrophes.

Impact: paths containing `'` reload incorrectly.

Fix:

- Unescape doubled apostrophes in single-quoted YAML.
- Or avoid custom YAML escaping and use a proper YAML writer/parser.

6. Clarify unused command pattern API

`commands/dangerous.ts` supports compiled patterns and fallback patterns, but `evaluateBash` passes empty arrays.

Impact:

- Dead/unclear API surface.
- Tests cover behavior not currently used by extension config.

Fix:

- Wire compiled patterns into config flow.
- Or remove/de-scope unused API and tests.

7. Split `evaluateBash` into ordered evaluators

`shell/analysis.ts` handles explicit regex rules, built-ins, path rules, mutating checks, deleting checks, and strict mode in one function.

Impact:

- Policy precedence is harder to reason about.
- Future rules are harder to add safely.

Fix:

- Keep public `evaluateBash`.
- Internally split into small ordered checks:
- configured bash patterns
- builtin dangerous commands
- zero-access path references
- read-only mutation
- no-delete deletion
- strict mode allowlist

8. Add focused test coverage

Current tests mostly cover `commands/dangerous.ts`.

Missing useful tests:

- symlink path policy matching
- outside-cwd extraction
- YAML persistence and reload
- `multi_tool_use.parallel` payload parsing
- Docker/Podman separated volume flags
- shared path prompt grant behavior

## Optional

9. Add extension-local package metadata

`web-tools` has its own `package.json` and `tsconfig.json`; `damage-control` does not.

Benefit:

- Easier local typecheck/test commands.
- Clearer extension boundary.
- Better workspace consistency.

Possible scripts:

- `test`
- `typecheck`
- `check`

10. Normalize event/log payloads

`tool_call` and `user_bash` emit similar but not identical block/log data.

Benefit:

- Easier external consumption.
- More reliable audit trail.
- Simpler tests.

Fix:

- Define typed log entry union.
- Reuse event emit helpers.
- Keep metadata shape consistent.

## Suggested Execution Order

1. Type-safe parser for `multi_tool_use.parallel`.
2. Container matcher improvements plus tests.
3. Symlink-aware path matching plus tests.
4. YAML quote reload fix plus tests.
5. Extract shared outside-cwd helpers.
6. Clean up command pattern API.
7. Add extension-local check scripts if wanted.
