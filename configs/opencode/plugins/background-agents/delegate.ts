import { type ToolContext, tool } from "@opencode-ai/plugin";
import type { DelegationManager } from "./delegationManager";

type DelegateArgs = {
	prompt: string;
	agent: string;
};

export function createDelegate(
	manager: DelegationManager,
): ReturnType<typeof tool> {
	return tool({
		description: `Delegate a task to an agent. Returns immediately with a readable ID.

Use this for:
- Research tasks (will be auto-saved)
- Parallel work that can run in background
- Any task where you want persistent, retrievable output

On completion, a notification will arrive with the ID and terminal summary.
Use \`delegation_read\` with the ID to retrieve full persisted output (including after compaction).`,
		args: {
			prompt: tool.schema
				.string()
				.describe(
					"The full detailed prompt for the agent. Must be in English.",
				),
			agent: tool.schema
				.string()
				.describe(
					'Agent to delegate to. Must be a read-only sub-agent (edit/write/bash denied), such as "researcher" or "explore".',
				),
		},
		async execute(args: DelegateArgs, toolCtx: ToolContext): Promise<string> {
			if (!toolCtx?.sessionID) {
				return "❌ delegate requires sessionID. This is a system error.";
			}
			if (!toolCtx?.messageID) {
				return "❌ delegate requires messageID. This is a system error.";
			}

			try {
				const delegation = await manager.delegate({
					parentSessionID: toolCtx.sessionID,
					parentMessageID: toolCtx.messageID,
					parentAgent: toolCtx.agent,
					prompt: args.prompt,
					agent: args.agent,
				});

				// Get total active count for this parent session
				const pendingSet = manager.getPendingCount(toolCtx.sessionID);
				const totalActive = pendingSet;

				let response = `Delegation started: ${delegation.id}\nAgent: ${args.agent}`;
				if (totalActive > 1) {
					response += `\n\n${totalActive} delegations now active.`;
				}
				response += `\nYou WILL be notified when ${totalActive > 1 ? "ALL complete" : "complete"}. Do NOT poll.`;

				return response;
			} catch (error) {
				// Return validation errors as guidance, not exceptions
				return `❌ Delegation failed:\n\n${error instanceof Error ? error.message : "Unknown error"}`;
			}
		},
	});
}

export function createDelegationRead(
	manager: DelegationManager,
): ReturnType<typeof tool> {
	return tool({
		description: `Read the output of a delegation by its ID.
Use this to retrieve results from delegated tasks if the inline notification was lost during compaction.`,
		args: {
			id: tool.schema
				.string()
				.describe("The delegation ID (e.g., 'elegant-blue-tiger')"),
		},
		async execute(args: { id: string }, toolCtx: ToolContext): Promise<string> {
			if (!toolCtx?.sessionID) {
				return "❌ delegation_read requires sessionID. This is a system error.";
			}

			return await manager.readOutput(toolCtx.sessionID, args.id);
		},
	});
}

export function createDelegationList(
	manager: DelegationManager,
): ReturnType<typeof tool> {
	return tool({
		description: `List all delegations for the current session.
Shows both running and completed delegations.`,
		args: {},
		async execute(
			_args: Record<string, never>,
			toolCtx: ToolContext,
		): Promise<string> {
			if (!toolCtx?.sessionID) {
				return "❌ delegation_list requires sessionID. This is a system error.";
			}

			const delegations = await manager.listDelegations(toolCtx.sessionID);

			if (delegations.length === 0) {
				return "No delegations found for this session.";
			}

			const lines = delegations.map((d) => {
				const titlePart = d.title ? ` | ${d.title}` : "";
				const unreadPart = d.unread ? " [unread]" : "";
				const descPart = d.description ? `\n  → ${d.description}` : "";
				return `- **${d.id}**${titlePart} [${d.status}]${unreadPart}${descPart}`;
			});

			return `## Delegations\n\n${lines.join("\n")}`;
		},
	});
}
