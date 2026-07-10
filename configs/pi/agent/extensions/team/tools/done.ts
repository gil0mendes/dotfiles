import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { Runtime } from "../runtime";
import Type from "typebox";
import { getToolContext, toolError, toolSuccess } from "./shared";

export const registerTeamDoneTool = (pi: ExtensionAPI, runtime: Runtime) =>
	pi.registerTool({
		name: "team_done",
		label: "Done with team",
		description: "Close a waiting interactive subagent.",
		parameters: Type.Object({
			subagentId: Type.String({ description: "ID of the subagent to close" }),
		}),
		promptSnippet: "Close a waiting interactive subagent.",
		promptGuidelines: [
			"team_done: Use only when no further follow-up is needed.",
		],
		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const toolCtx = getToolContext(ctx);
			const { error } = runtime.done(
				params.subagentId,
				toolCtx.callerSessionId,
			);

			if (error) {
				return toolError(error);
			}

			return toolSuccess(`Subagent ${params.subagentId} closed.`, {
				id: params.subagentId,
			});
		},
	});
