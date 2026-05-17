# Todos Pi Extension

A file-backed todo manager for Pi, installed from:

<https://github.com/mitsuhiko/agent-stuff/blob/main/extensions/todos.ts>

## What it adds

- A `todo` tool the agent can use to manage tasks.
- A `/todos` command that opens an interactive todo manager in Pi.
- Markdown todo files stored under `.pi/todos` in the current project by default.
- Optional task assignment to the current Pi session to reduce conflicts.
- Lock files while a todo is being edited.
- Garbage collection for old closed todos.

## Storage

By default, todos are stored in:

```text
.pi/todos/
```

Set `PI_TODO_PATH` to use a different path:

```bash
export PI_TODO_PATH=/path/to/todos
```

Each todo is a standalone Markdown file named `<id>.md`. The file starts with a JSON metadata block, followed by optional Markdown notes.

Example:

```markdown
{
  "id": "deadbeef",
  "title": "Add tests",
  "tags": ["qa"],
  "status": "open",
  "created_at": "2026-01-25T17:00:00.000Z",
  "assigned_to_session": "session-id"
}

Notes about the work go here.
```

## Agent tool actions

The `todo` tool supports:

- `list` — list open and assigned todos
- `list-all` — list open, assigned, and closed todos
- `get` — read one todo
- `create` — create a todo
- `update` — update title, status, tags, or body
- `append` — append Markdown notes to the body
- `claim` — assign a todo to the current session
- `release` — remove an assignment
- `delete` — delete a todo file

Todo IDs are shown as `TODO-<hex>`. Tool calls accept either `TODO-<hex>` or the raw hex ID.

## Slash command

Run this inside Pi:

```text
/todos
```

In interactive mode it opens a searchable UI. From there you can view, work on, refine, close/reopen, release, delete, and copy todos.

## Settings

The extension reads optional settings from:

```text
.pi/todos/settings.json
```

Defaults:

```json
{
  "gc": true,
  "gcDays": 7
}
```

When `gc` is enabled, closed todos older than `gcDays` are deleted on startup.

## Reloading

This extension is installed at:

```text
~/.pi/agent/extensions/todos/index.ts
```

Pi auto-discovers this location. If Pi is already running, use:

```text
/reload
```
