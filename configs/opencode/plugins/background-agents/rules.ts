const DELEGATION_RULES = `<task-notification>
<delegation-system>

## Async Delegation

You have tools for parallel background work:
- \`delegate(prompt, agent)\` - Launch task, returns ID immediately
- \`delegation_read(id)\` - Retrieve completed result
- \`delegation_list()\` - List delegations (use sparingly)

## Delegation Routing

Agents route based on their permissions:

| Agent Type | Tool | Why |
|------------|------|-----|
| Read-only (researcher, explore) | \`delegate\` | Background session, async |
| Write-capable (coder, scribe) | \`task\` | Native task, preserves undo/branching |

**Read-only agents** have edit="deny", write="deny", bash={"*":"deny"}.
**Write-capable agents** have any write tool enabled.

## How It Works

1. For read-only agents: Call \`delegate\` with detailed prompt
2. For write-capable agents: Call \`task\` with detailed prompt
3. Continue productive work while it runs
4. Receive notification when complete
5. Call \`delegation_read(id)\` to retrieve results

## Critical Constraints

**NEVER poll \`delegation_list\` to check completion.**
You WILL be notified via \`<task-notification>\`. Polling wastes tokens.

**NEVER wait idle.** Always have productive work while delegations run.

**Using wrong tool will fail fast with guidance.**

</delegation-system>
</task-notification>`;

/**
 * Expected input for experimental.chat.system.transform hook.
 */
type SystemTransformInput = {
	agent?: string;
	sessionID?: string;
};

export const injectDelegationRules = async (
	_input: SystemTransformInput,
	output,
) => {
	output.system.push(DELEGATION_RULES);
};
