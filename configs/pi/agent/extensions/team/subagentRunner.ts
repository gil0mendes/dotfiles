import type { SettledSubagentStatus, SubAgentState } from "./types";

export interface SubagentRunner {}

export type SubagentRunnerCallbacks = {
	isCurrent: (state: SubAgentState) => boolean;
	onProgress: (ownerSessionId: string) => void;
	onSettled: (
		state: SubAgentState,
		status: SettledSubagentStatus,
		outcome: { result?: string; error?: string },
	) => void;
};

export class SubagentSessionRunner implements SubagentRunner {}
