import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { SubagentStatus } from "../types";

export type SteeringPayload = {
	id: string;
	agentName: string;
	sessionFile?: string;
	status: SubagentStatus;
	result?: string;
	error?: string;
};

export type SteeringMessagePayload = {
	message: Parameters<ExtensionAPI["sendMessage"]>[0];
	options?: Parameters<ExtensionAPI["sendMessage"]>[1];
};

export const STATUS_ICON: Record<SubagentStatus, string> = {
	running: "⏳",
	waiting: "⏳",
	done: "✅",
	error: "❌",
	aborted: "⏹️",
};

export const STATUS_LABEL: Record<SubagentStatus, string> = {
	running: "running",
	waiting: "waiting for response",
	done: "done",
	error: "failed",
	aborted: "aborted",
};

const getSteeringBody = (payload: SteeringPayload): string | undefined =>
	payload.status === "error" || payload.status === "aborted"
		? (payload.error ?? payload.result)
		: (payload.result ?? payload.error);

const getTeamResultTitle = (payload: SteeringPayload): string =>
	`Subagent '${payload.agentName}' (${payload.id}) ${STATUS_LABEL[payload.status]}`;

export const getSteeringMessagePayload = (
	payload: SteeringPayload,
	opts: { isIdle: boolean; triggerTurn: boolean },
): SteeringMessagePayload => {
	const title = getTeamResultTitle(payload);
	const body = getSteeringBody(payload);
	const content = body
		? `**${STATUS_ICON[payload.status]} ${title}**\n\n${body}`
		: `**${STATUS_ICON[payload.status]} ${title}**`;

	const options: Parameters<ExtensionAPI["sendMessage"]>[1] = opts.isIdle
		? { triggerTurn: opts.triggerTurn }
		: {
				deliverAs: "steer",
				triggerTurn: opts.triggerTurn,
			};

	return {
		message: {
			customType: "team-result",
			content,
			display: true,
			details: {
				agentId: payload.id,
				agentName: payload.agentName,
				sessionFile: payload.sessionFile,
				status: payload.status,
				body,
			},
		},
		options,
	};
};
