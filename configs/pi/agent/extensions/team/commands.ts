import type { ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import type { ActiveAgentSummary, Runtime } from "./runtime";

export type TeamCommandAction = "menu" | "list" | "respond" | "abort";

const actionDescriptions: Record<TeamCommandAction, string> = {
	menu: "Show available team actions",
	list: "List active agents",
	respond: "Respond to a waiting agent",
	abort: "Abort active agents",
};

const formatAgent = (agent: ActiveAgentSummary): string =>
	`${agent.agentName} (${agent.status}) · ${agent.id}`;

const summarizeAgents = (agents: ActiveAgentSummary[]): string => {
	if (agents.length === 0) return "No active agents in this session.";
	return ["Active agents:", ...agents.map((agent) => `• ${formatAgent(agent)}`)].join("\n");
};

export const parseTeamCommandAction = (args: string): TeamCommandAction | undefined => {
	const action = args.trim().toLowerCase();
	if (!action) return "menu";
	if (action === "list" || action === "respond" || action === "abort") {
		return action;
	}
	return undefined;
};

const notify = (
	ctx: ExtensionCommandContext,
	message: string,
	type: "info" | "warning" | "error" = "info",
): void => {
	ctx.ui.notify(message, type);
};

const selectAgent = async (
	ctx: ExtensionCommandContext,
	title: string,
	agents: ActiveAgentSummary[],
): Promise<ActiveAgentSummary | undefined> => {
	const labels = agents.map(formatAgent);
	const selected = await ctx.ui.select(title, labels);
	if (!selected) return undefined;
	return agents.find((agent) => formatAgent(agent) === selected);
};

const respondToWaitingAgent = async (
	ctx: ExtensionCommandContext,
	runtime: Runtime,
	sessionId: string,
): Promise<void> => {
	const waitingAgents = runtime
		.getActiveSummariesForOwner(sessionId)
		.filter((agent) => agent.status === "waiting");
	if (waitingAgents.length === 0) {
		notify(ctx, "No agents are waiting for a response.", "info");
		return;
	}

	const agent = await selectAgent(ctx, "Respond to waiting agent", waitingAgents);
	if (!agent) return;

	const response = await ctx.ui.input(`Response for ${agent.agentName}`, "Message");
	const message = response?.trim();
	if (!message) {
		notify(ctx, "Response cancelled; no message was sent.", "info");
		return;
	}

	const { error } = runtime.respond(agent.id, message, sessionId);
	if (error) {
		notify(ctx, error, "warning");
		return;
	}
	notify(ctx, `Message sent to ${agent.agentName}.`, "info");
};

const abortAgents = async (
	ctx: ExtensionCommandContext,
	runtime: Runtime,
	sessionId: string,
): Promise<void> => {
	const agents = runtime.getActiveSummariesForOwner(sessionId);
	if (agents.length === 0) {
		notify(ctx, "No active agents to abort in this session.", "info");
		return;
	}

	const abortAllLabel = "All active agents";
	const selected = await ctx.ui.select("Abort agent", [abortAllLabel, ...agents.map(formatAgent)]);
	if (!selected) return;

	const selectedAgent = agents.find((agent) => formatAgent(agent) === selected);
	if (selected !== abortAllLabel && !selectedAgent) {
		notify(ctx, "Selected agent is no longer active in this session.", "warning");
		return;
	}

	const targetDescription = selectedAgent ? formatAgent(selectedAgent) : abortAllLabel;
	const confirmed = await ctx.ui.confirm(
		"Abort team agent?",
		`Abort ${targetDescription}? This cannot be undone.`,
	);
	if (!confirmed) return;

	const abortReason = "Aborted from /team";
	const abortedIds = selectedAgent
		? runtime.abortOwned(selectedAgent.id, sessionId, { reason: abortReason })
			? [selectedAgent.id]
			: []
		: runtime.abortAllOwnedBy(sessionId, { reason: abortReason });

	if (abortedIds.length === 0) {
		notify(ctx, "Selected agents are no longer active in this session.", "warning");
		return;
	}
	notify(ctx, `Aborted ${abortedIds.length} agent(s): ${abortedIds.join(", ")}.`, "info");
};

const runTeamCommand = async (
	ctx: ExtensionCommandContext,
	runtime: Runtime,
	action: TeamCommandAction,
): Promise<void> => {
	const sessionId = ctx.sessionManager.getSessionId();
	if (action === "list") {
		notify(ctx, summarizeAgents(runtime.getActiveSummariesForOwner(sessionId)));
		return;
	}
	if (action === "respond") {
		await respondToWaitingAgent(ctx, runtime, sessionId);
		return;
	}
	if (action === "abort") {
		await abortAgents(ctx, runtime, sessionId);
		return;
	}

	const agents = runtime.getActiveSummariesForOwner(sessionId);
	notify(ctx, summarizeAgents(agents));
	if (agents.length === 0) return;

	const selected = await ctx.ui.select("Team actions", [
		actionDescriptions.respond,
		actionDescriptions.abort,
	]);
	if (selected === actionDescriptions.respond) {
		await respondToWaitingAgent(ctx, runtime, sessionId);
		return;
	}
	if (selected === actionDescriptions.abort) {
		await abortAgents(ctx, runtime, sessionId);
	}
};

export const registerTeamCommand = (pi: ExtensionAPI, runtime: Runtime): void => {
	pi.registerCommand("team", {
		description: "List active team agents, respond to waiting agents, or abort agents",
		handler: async (args, ctx) => {
			const action = parseTeamCommandAction(args);
			if (!action) {
				notify(ctx, "Usage: /team [list|respond|abort]", "warning");
				return;
			}
			if (!ctx.hasUI) {
				return;
			}
			await runTeamCommand(ctx, runtime, action);
		},
	});
};
