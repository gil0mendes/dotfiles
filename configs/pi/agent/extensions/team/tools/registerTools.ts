import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { Runtime } from "../runtime";
import { registerTeamAbortTool } from "./abort";
import { registerTeamListTool } from "./list";
import { registerTeamRespondTool } from "./respond";
import { registerTeamSpawnTool } from "./spawn";
import { registerTeamDoneTool } from "./done";

export const setupTools = (pi: ExtensionAPI, runtime: Runtime): void => {
	registerTeamListTool(pi, runtime);
	registerTeamSpawnTool(pi, runtime);
	registerTeamRespondTool(pi, runtime);
	registerTeamAbortTool(pi, runtime);
	registerTeamDoneTool(pi, runtime);
};
