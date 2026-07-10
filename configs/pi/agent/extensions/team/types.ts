import type { AgentSession } from "@earendil-works/pi-coding-agent";
import type { AgentConfig } from "./loader/types";

export type SubagentStatus =
	"running" | "waiting" | "done" | "error" | "aborted";

export type SettledSubagentStatus = Extract<
	SubagentStatus,
	"done" | "waiting" | "error" | "aborted"
>;

export type SubagentState = {
	id: string;
	agentConfig: AgentConfig;
	task: string;
	brief: string;
	status: SubagentStatus;
	ownerSessionId: string;
	session: AgentSession | null;
	turns: number;
	contextTokens: number;
	model?: string;
	error?: string;
	result?: string;
	unsubscribe?: () => void;
};

export interface SteeringPayload {
	id: string;
	agentName: string;
	sessionFile?: string;
	status: SubagentStatus;
	result?: string;
	error?: string;
}

export type PendingMessage = {
	ownerSessionId: string;
	payload: SteeringPayload;
	queuedAt: number;
};
