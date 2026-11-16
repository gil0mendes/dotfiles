# Progressive Disclosure

Token-efficient skill design through staged information loading.

## Three-Tier Loading Model

The Agent Skills client guide describes three tiers. Check the target client's discovery, filtering, and activation behavior.

```
Tier 1: Metadata (~100 tokens)
├── name + description
├── May be exposed during skill discovery
└── Used to decide: "Is this skill relevant?"

Tier 2: SKILL.md Body (<5000 tokens recommended)
├── Full instructions
├── May be loaded when the skill is invoked
└── Contains navigation to Tier 3

Tier 3: Bundled Resources (on-demand)
├── references/, scripts/, assets/
├── Can be read when needed
└── Context cost depends on how the harness accesses them
```

## Context Window Economics

| Scenario                  | Files Loaded        | ~Tokens |
| ------------------------- | ------------------- | ------- |
| Agent startup (10 skills) | Metadata only       | ~1000   |
| Skill triggered           | + SKILL.md          | +1500   |
| Read 1 reference          | + reference file    | +800    |
| Full implementation       | SKILL.md + 2-3 refs | ~4000   |
| **Worst case**            | Everything          | 10000+  |

**Key insight:** Clear navigation can reduce context load; actual costs depend on the harness and task.

## File Decomposition Strategy

### Single-File vs Multi-File Decision

```
Keep in single SKILL.md when:
├─ Total content < 200 lines
├─ All content needed together
└─ No distinct "optional" sections

Split into multiple files when:
├─ Content > 200 lines
├─ Different tasks need different content
├─ Sections are mutually exclusive
└─ Large reference tables/schemas
```

### The 5-File Pattern

For complex products/domains:

```
product/
├── README.md           # Overview, quick start (always read first)
├── api.md              # Runtime APIs, types
├── configuration.md    # Setup, config files
├── patterns.md         # Best practices, examples
└── gotchas.md          # Pitfalls, debugging
```

**Task-based loading:**

| Task              | Files to Read           |
| ----------------- | ----------------------- |
| Quick start       | README.md only          |
| Implement feature | README + api + patterns |
| Configure/setup   | README + configuration  |
| Debug issue       | gotchas.md              |

**Token savings:** 2-3K per task vs 5-6K reading everything.

## Cross-Referencing

### Do: Link Without Duplicating

```markdown
## Quick Start

Basic usage here...

## Advanced Features

**Form filling**: See [forms.md](./references/forms.md)
**Batch processing**: See [batch.md](./references/batch.md)
```

The pointers guide the agent to forms.md or batch.md when needed; loading behavior depends on the harness.

### Don't: Duplicate Content

```markdown
## Quick Start

Here's how to process PDFs...

## Advanced Features

As mentioned above, to process PDFs... # BAD: duplicates content
```

## Navigation Patterns

### "In This Reference" Section

For skills with several references, consider an index in SKILL.md:

```markdown
## In This Reference

| File                                    | Purpose         |
| --------------------------------------- | --------------- |
| [api.md](./references/api.md)           | Runtime APIs    |
| [config.md](./references/config.md)     | Setup           |
| [patterns.md](./references/patterns.md) | Best practices  |
| [gotchas.md](./references/gotchas.md)   | Troubleshooting |
```

### "Reading Order" Section

Guide task-based navigation:

```markdown
## Reading Order

| Task        | Files                   |
| ----------- | ----------------------- |
| New project | README + config         |
| Add feature | README + api + patterns |
| Debug       | gotchas                 |
```

### Decision Trees for Routing

For large skills:

```
What do you need?
├─ Store data → See storage/
│   ├─ Key-value → kv/
│   ├─ SQL → d1/
│   └─ Objects → r2/
├─ Run code → See compute/
│   ├─ Serverless → workers/
│   └─ Stateful → durable-objects/
```

Agent routes to correct product without reading everything.

## Anti-Patterns

| Pattern              | Problem          | Fix                    |
| -------------------- | ---------------- | ---------------------- |
| Monolithic SKILL.md  | Context rot      | Split by task/domain   |
| Duplicated content   | Staleness, bloat | Link, don't copy       |
| Missing navigation   | Agent guesses    | Add decision trees     |
| Everything in Tier 2 | Wasted tokens    | Push details to Tier 3 |

## See Also

- [anatomy.md](./anatomy.md) - Directory structures
- [patterns.md](./patterns.md) - Real skill patterns
- [gotchas.md](./gotchas.md) - Common mistakes
