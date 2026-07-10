import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import {
	isTeamRuntimeChangedPayload,
	isTeamWarningPayload,
	TEAM_RUNTIME_CHANGED_EVENT,
	TEAM_WARNING_EVENT,
} from "../events";
import type { ActiveAgentSummary, Runtime } from "../runtime";

const WIDGET_ID = "team-subagents";
const STATUS_ID = "team-subagents";

const STATUS_ICON: Record<ActiveAgentSummary["status"], string> = {
	running: "●",
	waiting: "◌",
	done: "✓",
	error: "×",
	aborted: "■",
};

const renderWidget = (ctx: ExtensionContext, runtime: Runtime): void => {
	const sessionId = ctx.sessionManager.getSessionId();
	const agents = runtime.getActiveSummariesForOwner(sessionId);

	if (agents.length === 0) {
		ctx.ui.setWidget(WIDGET_ID, undefined);
		ctx.ui.setStatus(STATUS_ID, undefined);
		return;
	}

	const lines = agents.map((agent) => {
		const icon = STATUS_ICON[agent.status];
		const detail = agent.contextTokens
			? ` · ${agent.turns} turns · ${agent.contextTokens} tokens`
			: ` · ${agent.turns} turns`;
		const label = `${icon} ${agent.agentName} (${agent.status})${detail}`;
		return ctx.ui.theme.fg(agent.status === "waiting" ? "warning" : "accent", label);
	});

	ctx.ui.setWidget(WIDGET_ID, lines);
	ctx.ui.setStatus(STATUS_ID, ctx.ui.theme.fg("accent", `◉ ${agents.length} subagent(s)`));
};

export function registerUIRenderers(pi: ExtensionAPI, runtime: Runtime): void {
	let currentCtx: ExtensionContext | undefined;

	const unsubscribeRuntimeChanges = pi.events.on(TEAM_RUNTIME_CHANGED_EVENT, (event) => {
		if (!currentCtx || !isTeamRuntimeChangedPayload(event)) return;
		if (event.ownerSessionId !== currentCtx.sessionManager.getSessionId()) return;
		renderWidget(currentCtx, runtime);
	});

	const unsubscribeWarnings = pi.events.on(TEAM_WARNING_EVENT, (event) => {
		if (!currentCtx || !isTeamWarningPayload(event)) return;
		if (event.ownerSessionId !== currentCtx.sessionManager.getSessionId()) return;
		currentCtx.ui.notify(event.message, "warning");
	});

	pi.on("session_start", (_event, ctx) => {
		currentCtx = ctx;
		renderWidget(ctx, runtime);
	});

	pi.on("session_shutdown", (_event, ctx) => {
		if (currentCtx?.sessionManager.getSessionId() !== ctx.sessionManager.getSessionId()) {
			return;
		}
		ctx.ui.setWidget(WIDGET_ID, undefined);
		ctx.ui.setStatus(STATUS_ID, undefined);
		currentCtx = undefined;
		unsubscribeRuntimeChanges();
		unsubscribeWarnings();
	});

	pi.registerMessageRenderer("team-result", (message, _options, theme) => {
		const content = typeof message.content === "string" ? message.content : "";
		return new Text(theme.fg("accent", content), 0, 0);
	});
}
