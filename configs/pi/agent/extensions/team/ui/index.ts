import type { EventBus, ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
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
const MAX_VISIBLE_AGENTS = 4;
const MAX_AGENT_NAME_LENGTH = 24;
const MAX_MODEL_LABEL_LENGTH = 20;

type AgentStatusPresentation = {
	rows: string[];
	footer: string;
	hasRunningAgents: boolean;
};

const STATUS_ICON: Record<ActiveAgentSummary["status"], string> = {
	running: "●",
	waiting: "◌",
	done: "✓",
	error: "×",
	aborted: "■",
};

const SPINNER_FRAMES = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

const truncate = (value: string, maxLength: number): string => {
	if (value.length <= maxLength) return value;
	return `${value.slice(0, Math.max(0, maxLength - 1))}…`;
};

export const formatElapsed = (startedAt: number, now: number): string => {
	const elapsedSeconds = Math.max(0, Math.floor((now - startedAt) / 1000));
	const minutes = Math.floor(elapsedSeconds / 60);
	const seconds = elapsedSeconds % 60;
	if (minutes === 0) return `${seconds}s`;
	return `${minutes}m${String(seconds).padStart(2, "0")}s`;
};

export const buildAgentStatusPresentation = (
	agents: ActiveAgentSummary[],
	now: number,
	spinnerFrame: string,
): AgentStatusPresentation => {
	const waitingCount = agents.filter((agent) => agent.status === "waiting").length;
	const visibleAgents = agents.slice(0, MAX_VISIBLE_AGENTS);
	const rows = visibleAgents.map((agent) => {
		const icon = agent.status === "running" ? spinnerFrame : STATUS_ICON[agent.status];
		const name = truncate(agent.agentName, MAX_AGENT_NAME_LENGTH);
		const progress = agent.contextTokens
			? `${agent.turns}t · ${agent.contextTokens} tok`
			: `${agent.turns}t`;
		const model = agent.model ? ` · ${truncate(agent.model, MAX_MODEL_LABEL_LENGTH)}` : "";
		return truncate(
			`${icon} ${name}${model} · ${agent.status} · ${progress} · ${formatElapsed(agent.startedAt, now)}`,
			80,
		);
	});

	if (agents.length > visibleAgents.length) {
		rows.push(`… ${agents.length - visibleAgents.length} more active agent(s)`);
	}

	const waitingSuffix = waitingCount > 0 ? ` · ${waitingCount} waiting` : "";
	const hasRunningAgents = agents.some((agent) => agent.status === "running");
	return {
		rows,
		footer: `${hasRunningAgents ? spinnerFrame : "◉"} ${agents.length} agent(s)${waitingSuffix}`,
		hasRunningAgents,
	};
};

const renderWidget = (ctx: ExtensionContext, runtime: Runtime, spinnerFrame: string): boolean => {
	const sessionId = ctx.sessionManager.getSessionId();
	const agents = runtime.getActiveSummariesForOwner(sessionId);

	if (agents.length === 0) {
		ctx.ui.setWidget(WIDGET_ID, undefined);
		ctx.ui.setStatus(STATUS_ID, undefined);
		return false;
	}

	const presentation = buildAgentStatusPresentation(agents, Date.now(), spinnerFrame);
	const lines = presentation.rows.map((row, index) => {
		if (index >= MAX_VISIBLE_AGENTS) return ctx.ui.theme.fg("muted", row);
		const agent = agents[index];
		if (!agent) return ctx.ui.theme.fg("muted", row);
		return ctx.ui.theme.fg(agent.status === "waiting" ? "warning" : "accent", row);
	});

	ctx.ui.setWidget(WIDGET_ID, lines, { placement: "aboveEditor" });
	ctx.ui.setStatus(
		STATUS_ID,
		ctx.ui.theme.fg(waitingCount(agents) > 0 ? "warning" : "accent", presentation.footer),
	);
	return presentation.hasRunningAgents;
};

const waitingCount = (agents: ActiveAgentSummary[]): number =>
	agents.filter((agent) => agent.status === "waiting").length;

type TeamEventHandlers = {
	onRuntimeChanged: (event: unknown) => void;
	onWarning: (event: unknown) => void;
};

export const createTeamEventSubscriptionLifecycle = (
	events: EventBus,
	handlers: TeamEventHandlers,
) => {
	let unsubscribe: (() => void) | undefined;

	const subscribe = (): void => {
		if (unsubscribe) return;
		const unsubscribeRuntimeChanges = events.on(
			TEAM_RUNTIME_CHANGED_EVENT,
			handlers.onRuntimeChanged,
		);
		const unsubscribeWarnings = events.on(TEAM_WARNING_EVENT, handlers.onWarning);
		unsubscribe = () => {
			unsubscribeRuntimeChanges();
			unsubscribeWarnings();
			unsubscribe = undefined;
		};
	};

	const dispose = (): void => {
		unsubscribe?.();
	};

	return { subscribe, dispose };
};

export function registerUIRenderers(pi: ExtensionAPI, runtime: Runtime): void {
	let currentCtx: ExtensionContext | undefined;
	let spinnerIndex = 0;
	let elapsedTicker: ReturnType<typeof setInterval> | undefined;

	const clearElapsedTicker = (): void => {
		if (!elapsedTicker) return;
		clearInterval(elapsedTicker);
		elapsedTicker = undefined;
	};

	const refresh = (): void => {
		if (!currentCtx) return;
		const spinnerFrame = SPINNER_FRAMES[spinnerIndex] ?? SPINNER_FRAMES[0] ?? "●";
		const hasRunningAgents = renderWidget(currentCtx, runtime, spinnerFrame);
		if (!hasRunningAgents) clearElapsedTicker();
	};

	const ensureElapsedTicker = (): void => {
		if (elapsedTicker) return;
		elapsedTicker = setInterval(() => {
			if (!currentCtx) {
				clearElapsedTicker();
				return;
			}
			spinnerIndex = (spinnerIndex + 1) % SPINNER_FRAMES.length;
			refresh();
		}, 1000);
	};

	const refreshAndTick = (): void => {
		refresh();
		if (!currentCtx) return;
		const hasRunningAgents = runtime
			.getActiveSummariesForOwner(currentCtx.sessionManager.getSessionId())
			.some((agent) => agent.status === "running");
		if (hasRunningAgents) ensureElapsedTicker();
	};

	const eventSubscriptions = createTeamEventSubscriptionLifecycle(pi.events, {
		onRuntimeChanged: (event) => {
			if (!currentCtx || !isTeamRuntimeChangedPayload(event)) return;
			if (event.ownerSessionId !== currentCtx.sessionManager.getSessionId()) return;
			refreshAndTick();
		},
		onWarning: (event) => {
			if (!currentCtx || !isTeamWarningPayload(event)) return;
			if (event.ownerSessionId !== currentCtx.sessionManager.getSessionId()) return;
			currentCtx.ui.notify(event.message, "warning");
		},
	});

	pi.on("session_start", (_event, ctx) => {
		eventSubscriptions.dispose();
		currentCtx = ctx;
		eventSubscriptions.subscribe();
		refreshAndTick();
	});

	pi.on("session_shutdown", (_event, ctx) => {
		if (currentCtx?.sessionManager.getSessionId() !== ctx.sessionManager.getSessionId()) {
			return;
		}
		clearElapsedTicker();
		eventSubscriptions.dispose();
		ctx.ui.setWidget(WIDGET_ID, undefined);
		ctx.ui.setStatus(STATUS_ID, undefined);
		currentCtx = undefined;
	});

	pi.registerMessageRenderer("team-result", (message, _options, theme) => {
		const content = typeof message.content === "string" ? message.content : "";
		return new Text(theme.fg("accent", content), 0, 0);
	});
}
