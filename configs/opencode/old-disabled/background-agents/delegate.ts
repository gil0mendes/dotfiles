import type { Context } from "@opencode/plugin/promise/plugin";
import type { DelegationManager } from "./delegationManager";

function requiredString(input: unknown, key: string): string | undefined {
	if (typeof input !== "object" || input === null) return undefined;
	const record = Object.fromEntries(Object.entries(input));
	const value = record[key];
	return typeof value === "string" && value.trim().length > 0 ? value : undefined;
}

export async function registerDelegationTools(
	ctx: Context,
	manager: DelegationManager,
): Promise<void> {
	await ctx.tool.transform((editor) => {
		editor.add({
			name: "delegate",
			description: "Delegate a read-only subagent task in the background.",
			input: {
				type: "object",
				properties: {
					prompt: { type: "string" },
					agent: { type: "string" },
				},
				required: ["prompt", "agent"],
				additionalProperties: false,
			},
			async execute(input, context) {
				const prompt = requiredString(input, "prompt");
				const agent = requiredString(input, "agent");
				if (!prompt || !agent) {
					return { content: "❌ delegate requires non-empty prompt and agent." };
				}
				try {
					const delegation = await manager.delegate({
						parentSessionID: context.sessionID,
						parentMessageID: context.messageID,
						parentAgent: context.agent,
						prompt,
						agent,
					});
					return {
						content: `Delegation started: ${delegation.id}\nAgent: ${agent}\nYou will be notified when complete. Do not poll.`,
					};
				} catch (error) {
					return { content: `❌ Delegation failed:\n\n${error instanceof Error ? error.message : "Unknown error"}` };
				}
			},
		});
		editor.add({
			name: "delegation_read",
			description: "Read a completed delegation result by ID.",
			input: {
				type: "object",
				properties: { id: { type: "string" } },
				required: ["id"],
				additionalProperties: false,
			},
			async execute(input, context) {
				const id = requiredString(input, "id");
				if (!id) return { content: "❌ delegation_read requires an ID." };
				return { content: await manager.readOutput(context.sessionID, id) };
			},
		});
		editor.add({
			name: "delegation_list",
			description: "List delegations for the current session.",
			input: { type: "object", properties: {}, additionalProperties: false },
			async execute(_input, context) {
				const delegations = await manager.listDelegations(context.sessionID);
				if (delegations.length === 0) return { content: "No delegations found." };
				return {
					content: `## Delegations\n\n${delegations.map((delegation) => `- **${delegation.id}** [${delegation.status}]`).join("\n")}`,
				};
			},
		});
	});
}
