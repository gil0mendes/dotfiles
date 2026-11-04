import type { ActiveAgentSummary, RecentAgentSummary } from "../runtime";

export type FleetRow =
	| { kind: "active"; agent: ActiveAgentSummary }
	| { kind: "recent"; agent: RecentAgentSummary };

export type FleetView = {
	rows: FleetRow[];
	detachedActiveCount: number;
};

export type FleetAction =
	| { kind: "close" }
	| { kind: "respond"; agentId: string }
	| { kind: "abort"; agentId: string };
