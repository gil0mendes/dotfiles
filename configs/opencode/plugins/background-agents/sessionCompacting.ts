import type { DelegationManager } from "./delegationManager";

type DelegationForContext = {
	id: string;
	agent?: string;
	title?: string;
	description?: string;
	status: string;
	startedAt?: Date;
	prompt?: string;
};

/**
 * Format delegation context for injection during compaction.
 * Includes running delegations with notification reminder (only when running exist),
 * and recent completed delegations with full descriptions.
 */
function formatDelegationContext(
	running: DelegationForContext[],
	completed: DelegationForContext[],
): string {
	const sections: string[] = ["<delegation-context>"];

	// Running delegations (if any)
	if (running.length > 0) {
		sections.push("## Running Delegations");
		sections.push("");
		for (const d of running) {
			sections.push(`### \`${d.id}\`${d.agent ? ` (${d.agent})` : ""}`);
			if (d.startedAt) {
				sections.push(`**Started:** ${d.startedAt.toISOString()}`);
			}
			if (d.prompt) {
				const truncatedPrompt =
					d.prompt.length > 200 ? `${d.prompt.slice(0, 200)}...` : d.prompt;
				sections.push(`**Prompt:** ${truncatedPrompt}`);
			}
			sections.push("");
		}

		// Only include reminder when there ARE running delegations
		sections.push(
			"> **Note:** You WILL be notified via `<task-notification>` when delegations complete.",
		);
		sections.push(
			"> Do NOT poll `delegation_list` - continue productive work.",
		);
		sections.push("");
	}

	// Completed delegations (recent)
	if (completed.length > 0) {
		sections.push("## Recent Completed Delegations");
		sections.push("");
		for (const d of completed) {
			const statusEmoji =
				d.status === "complete"
					? "✅"
					: d.status === "error"
						? "❌"
						: d.status === "timeout"
							? "⏱️"
							: "🚫";
			sections.push(`### ${statusEmoji} \`${d.id}\``);
			sections.push(`**Title:** ${d.title || "(no title)"}`);
			sections.push(`**Status:** ${d.status}`);
			sections.push(`**Description:** ${d.description || "(no description)"}`);
			sections.push("");
		}
		sections.push(
			"> Use `delegation_list()` to see all delegations for this session.",
		);
		sections.push("");
	}

	sections.push("## Retrieval");
	sections.push(
		'Use `delegation_read("id")` to access full delegation output.',
	);
	sections.push("</delegation-context>");

	return sections.join("\n");
}

export const sessionCompacting =
	(manager: DelegationManager) =>
	async (
		input: { sessionID: string },
		output: { context: string[]; prompt?: string },
	) => {
		const rootSessionID = await manager.getRootSessionID(input.sessionID);

		// Get running delegations for this session tree
		const running = manager
			.getRunningDelegations()
			.filter(
				(d) =>
					d.parentSessionID === input.sessionID ||
					d.parentSessionID === rootSessionID,
			)
			.map((d) => ({
				id: d.id,
				agent: d.agent,
				title: d.title,
				description: d.description,
				status: d.status,
				startedAt: d.startedAt,
				prompt: d.prompt,
			}));

		// Get recent completed delegations (last 10)
		const allDelegations = await manager.listDelegations(input.sessionID);
		const completed = allDelegations
			.filter((d) => d.status !== "running")
			.slice(-10)
			.map((d) => ({
				id: d.id,
				agent: d.agent,
				title: d.title,
				description: d.description,
				status: d.status,
			}));

		// Early exit if nothing to inject
		if (running.length === 0 && completed.length === 0) return;

		output.context.push(formatDelegationContext(running, completed));
	};
