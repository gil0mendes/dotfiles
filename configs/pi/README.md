# pi

To install dependencies:

```bash
bun install
```

To run:

```bash
bun run index.ts
```

## Resource picker

Run `/extension-toggle` or press `Ctrl+Shift+E` to open the floating picker. Type to search, move with ↑/↓, and press Space to stage changes. Enter saves; Esc clears the search or cancels. When opened with the shortcut, run `/reload` after saving to apply the changes.

The picker manages local extensions, skills, prompts, and themes. Changes are saved to each resource’s existing global or trusted project scope. Use Pi’s `pi config` for package resources.

This project was created using `bun init` in bun v1.3.3. [Bun](https://bun.com) is a fast all-in-one JavaScript runtime.
