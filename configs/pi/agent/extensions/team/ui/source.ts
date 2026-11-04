import type { Runtime } from "../runtime";
import type { FleetView } from "./types";

export const getFleetView = (runtime: Runtime, ownerSessionId: string): FleetView => {
	const snapshot = runtime.getFleetSnapshotForOwner(ownerSessionId);
	return {
		rows: [
			...snapshot.active.map((agent) => ({ kind: "active" as const, agent })),
			...snapshot.recent.map((agent) => ({ kind: "recent" as const, agent })),
		],
		detachedActiveCount: snapshot.detachedActiveCount,
	};
};
