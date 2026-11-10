# Team conversation viewer

Use `/team fleet` to open the team roster. Select an active or recent row and press `Enter` or `v` to inspect its conversation. Press `Escape`, `q`, or `Ctrl-C` to return to the fleet at the same selected row and window.

In the viewer:

- Use the arrow keys or `j`/`k` to scroll; use `PageUp`/`PageDown` for a page at a time.
- `Home` jumps to the beginning. `End` jumps to the latest content and resumes following new output. Scrolling pauses follow-tail until `End` is pressed.
- `Ctrl-O` or `t` expands/collapses tool arguments and output.
- `m` toggles between Markdown rendering and raw text.

The viewer is read-only: it does not provide mid-run steering, respond, or abort actions. Return to the fleet to use its existing controls: `r` requests a response for a waiting agent, and `a` asks for confirmation before aborting an active running or waiting agent. These actions retain the existing team runtime behavior. The viewer has no mouse or fullscreen-specific controls.

Conversation capture and recent transcripts are kept in memory only; transcripts are not persisted or reloaded from session history. Capture is bounded to 200 items, 256 KiB per transcript, and 16 KiB per field. Recent transcripts expire after 30 seconds and are capped at 20 per owner, 100 globally, and 8 MiB globally. Access is restricted to the owning session. Agents spawned before the capture implementation is loaded (including before a runtime reload) may have no captured history; the viewer does not reconstruct their earlier conversation retroactively.

# Team model preferences

Team agents can declare an ordered `model` preference as a string or list of strings in agent frontmatter or in `team.yaml`. A preference can be a bare query or a provider-qualified `provider/model-id` selector. The examples below use illustrative model IDs; availability depends on the child runtime.

The same ordered preferences can be written in either location:

```yaml
# Agent frontmatter, between the --- delimiters
model:
  - "claude sonnet 4_5"
  - "openrouter/anthropic/claude-sonnet-4.5"
  - "openai/gpt-x"
```

```yaml
# team.yaml override
agents:
  scout:
    model:
      - "claude sonnet 4_5"
      - "openrouter/anthropic/claude-sonnet-4.5"
      - "openai/gpt-x"
```

The list is tried in order. The first preference that resolves to one available model wins; an unavailable or ambiguous preference warns and selection continues to the next entry.

## Bare queries

A bare query such as `claude sonnet 4_5` is ranked against available model IDs and display names across providers. Provider names are not part of the score. Matching is case-insensitive; for normalized comparisons, runs of dots, hyphens, underscores, and whitespace are treated as one separator. The highest applicable rule wins (rules are not added together):

| Priority | Match |
| --- | --- |
| 100 | Exact model ID, ignoring case |
| 90 | Exact display name, ignoring case |
| 80 / 75 | Normalized exact model ID / display name |
| 70 | Query tokens are positional prefixes of the ID tokens; extra ID tokens may follow |
| 60 / 40 | Entire normalized query occurs in the ID / display name |
| 20 | Every query token occurs somewhere in the ID or display name; token order does not matter |

If multiple distinct models share the highest priority, the preference is ambiguous: it warns and selection tries the next preference. Registration order does not break ties, and a broad family query does not promise the newest model. Use a specific model ID when a particular dated version is required. Display-name and ranked substring matching apply only to bare queries.

## Provider-qualified selectors

A qualified selector stays within its named provider. Matching tries the literal provider-and-ID pair first, then a case-insensitive exact provider-and-ID match, then the legacy ordered token-prefix match on model IDs. The legacy match can skip candidate ID tokens, but query tokens must remain in order. It does not rank by display name or use bare-query substring rules. If multiple qualified fuzzy matches remain, the preference is ambiguous; another provider is never substituted.

Only the first slash separates provider from model ID, so the rest of the selector is preserved as the ID. For example, `openrouter/anthropic/claude-sonnet-4.5` names provider `openrouter` and the full ID `anthropic/claude-sonnet-4.5`.

The distinction matters for punctuation. Given available IDs `foo-bar` and `foo-x-bar` from provider `p`, qualified `p/foo.bar` matches both through legacy token prefixes and is ambiguous. Bare `foo.bar` normalizes to `foo bar`: `foo-bar` is a normalized exact ID match and ranks above `foo-x-bar`.

## Availability and fallback

Selection uses the authenticated models available to the child runtime. It does not use the parent's enabled-model list or `--models` cycling scope as its catalog.

If no preference resolves, the existing fallback remains: the parent's current model can be used only when that exact model is available to the child; otherwise startup fails. If there is no parent current model, the SDK may choose its normal available default. When fallback uses the parent's current model and its parent auth source is stored credentials, the child must have stored credentials or an explicitly inherited runtime key; environment or `models.json` fallback cannot substitute. A successful explicit preference is not treated as inherited current-model fallback and does not pass through that stored-source fallback guard.
