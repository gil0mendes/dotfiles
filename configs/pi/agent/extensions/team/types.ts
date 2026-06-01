export type SubagentStatus =
	| "running"
	| "waiting"
	| "done"
	| "error"
	| "aborted";

export type SettledSubagentStatus = Extract<
	SubagentStatus,
	"done" | "waiting" | "error" | "aborted"
>;

export type SubAgentState = {
	id: string;
};
