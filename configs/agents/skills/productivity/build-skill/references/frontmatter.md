# YAML Frontmatter Specification

Every SKILL.md must begin with YAML frontmatter.

## Required Format

```yaml
---
name: skill-name
description: What this skill does and when to use it.
---
```

**Critical:** Frontmatter must start at line 1. No blank lines before `---`.

## Required Fields

### name

| Constraint | Value                               |
| ---------- | ----------------------------------- |
| Required   | Yes                                 |
| Max length | 64 characters                       |
| Characters | Lowercase letters, numbers, hyphens |
| Must match | Directory name                      |

**Rules:**

- Lowercase letters, numbers, hyphens only
- Cannot start or end with hyphen
- No consecutive hyphens (`--`)
- Must match parent directory name

**Naming conventions:**

| Style                | Example           | When to Use          |
| -------------------- | ----------------- | -------------------- |
| Gerund (recommended) | `processing-pdfs` | Actions/capabilities |
| Noun phrase          | `pdf-processor`   | Tool/utility         |
| Domain-specific      | `cloudflare`      | Platform skills      |

**Good names:**

- `analyzing-data`
- `managing-releases`
- `code-review`
- `bigquery-analytics`

**Bad names:**

- `helper` (too vague)
- `utils` (meaningless)
- `MySkill` (uppercase)
- `pdf_processor` (underscores)

### description

| Constraint      | Value           |
| --------------- | --------------- |
| Required        | Yes             |
| Max length      | 1024 characters |
| Min recommended | 50 characters   |

**Rules:**

- Prefer clear, impersonal wording
- Include WHAT the skill does
- Include WHEN to use it (activation triggers)
- Be specific with key terms

**Good descriptions:**

```yaml
# PDF skill
description: Extract text and tables from PDF files, fill forms, merge documents. Use when working with PDF files or asked to read/edit PDFs.

# BigQuery skill
description: Query BigQuery datasets using SQL. Use for data analytics, reports, or Google Cloud data warehouse tasks.

# Code review skill
description: Review pull requests for quality, security, and test coverage. Use when asked to review a PR or diff.
```

**Bad descriptions:**

```yaml
description: Helps with files # Too vague
description: I can help you with data # Wrong POV
description: PDF tool # No trigger context
description: Useful utility # Meaningless
```

## Optional Fields

### license

```yaml
license: Apache-2.0
# or
license: See LICENSE.txt
```

### compatibility

```yaml
compatibility: Requires git, docker, and jq installed. macOS/Linux only.
```

Max 500 characters. Document environment requirements.

### metadata

```yaml
metadata:
  author: dmmulroy
  version: "1.0"
  team: platform
```

String-to-string map. Arbitrary key-value pairs.

### allowed-tools

The specification includes experimental `allowed-tools` for pre-approved tools. Support varies by client; consult the specification and client documentation before using it.

### Client extensions

Fields beyond the standard need client-specific support. The client guide mentions `disable-model-invocation` as an example of a model-driven activation opt-out, not a standard field or requirement. Check the target client before using it. Link bundled references from the Markdown body rather than assuming a `references` frontmatter field.

## Complete Example

```yaml
---
name: release-manager
description: Create releases with changelogs, version bumps, and git tags. Use when preparing a release, generating changelog, or bumping version numbers.
license: MIT
compatibility: Requires git and gh CLI
metadata:
  author: dmmulroy
  version: "2.0"
---
# Release Manager

Instructions here...
```

## Validation Checklist

| Check               | Requirement                                                |
| ------------------- | ---------------------------------------------------------- |
| Starts with `---`   | Line 1, no preceding blank lines                           |
| Has `name:`         | Required                                                   |
| Name format         | Lowercase, hyphens, no `--`, no leading/trailing `-`       |
| Name matches dir    | `my-skill/SKILL.md` has `name: my-skill`                   |
| Has `description:`  | Required                                                   |
| Description quality | Specific purpose and usage context recommended             |
| Closes with `---`   | Required                                                   |
| YAML validity       | Parse frontmatter as YAML; check specification constraints |

## See Also

- [anatomy.md](./anatomy.md) - Directory structures
- [gotchas.md](./gotchas.md) - Common frontmatter errors
