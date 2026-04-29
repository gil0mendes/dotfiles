---
name: workspace-detection
description: Determine workspace state and check for existing AI-DLC projects.
---

## Step 1: check for existing AI-DLC project

Check if `docs/ai/state.md` exists:

- **If exists:** resume from last state (load the state.md)
- **If not exists:** continue with new project assessment

## Step 2: scan workspace for existing code

**Determine if workspace has existing code:**

- Scan workspace for source code files (.java, .py, .js, .ts, .tsx, .go, .rs, .c, .h, etc.)
- Check for build files (package.json, build.gradle, etc.)
- Look for project structure indicators
- Identify workspace root directory (NOT docs/)

**Record findings:**

```markdown
## Workspsace State

- **Existing Code**: [Yes/No]
- **Programming Languages**: [List if found]
- **Build System**: [Maven/Gradle/npm/etc. if found]
- **Project Structure**: [Monolith/Microservices/Library/Empty]
- **Workspace Root**: [Absolute path]
```

## Step 3: determine next phase

**IF workspace is empty (no existing code)**:

- Set flag: `brownfield = false`
- Nothing else to do here

**IF workspace has existing code**:

- Set flag: `brownfield = true`
- Check for existing reverse engineering artifacts in `docs/ai/reverse-engineering/`
- **IF reverse engineering artifacts exist**:
  - Check if artifacts are stale (compare artifact timestamps against codebase's last significant modification)
  - **IF artifacts are current**: Load them, skip the rest
  - **IF artifacts are stale**: Load and execure the `reverse-engineering` skill (rerun to refresh artifacts)
  - **IF user explicitly requests rerun**: Load and execure the `reverse-engineering` skill regardless of staleness
- **IF no reverse engineering artifacts**: Load and execure the `reverse-engineering` skill
