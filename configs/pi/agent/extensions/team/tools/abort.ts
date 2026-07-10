import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import Type from "typebox";
import type { Runtime } from "../runtime";
import { getToolContext, toolError, toolSuccess } from "./shared";

export const registerTeamAbortTool = (
	pi: ExtensionAPI,
	runtime: Runtime,
): void => {
	pi.registerTool({
		name: "team_abort",
		label: "Abort team",
		description: "Abort active subagents owned by this session.",
		parameters: Type.Object({
			subagentId: Type.Optional(
				Type.String({ description: "Single subagent ID to abort" }),
			),
			subagentIds: Type.Optional(
				Type.Array(Type.String(), {
					minItems: 1,
					description: "List of subagent IDs to abort",
				}),
			),
			all: Type.Optional(
				Type.Boolean({
					description:
						"Abort all active subagents owned by the current session",
				}),
			),
		}),
		promptSnippet: "Abort active subagents.",
		promptGuidelines: [
			"team_abort: Use one mode only: subagentId, subagentIds, or all=true.",
		],
		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const { callerSessionId } = getToolContext(ctx);

			const modeCount =
				Number(Boolean(params.subagentId)) +
				Number(Boolean(params.subagentIds?.length)) +
				Number(params.all === true);
			if (modeCount !== 1) {
				return toolError(
					"Provide exactly one of: subagentId, subagentIds, or all=true.",
				);
			}

			const abortReason = "Aborted by tool request";
			const abortedIds = params.all
				? runtime.abortAllOwnedBy(callerSessionId, { reason: abortReason })
				: [];

			if (!params.all) {
				const requestedIds = params.subagentId
					? [params.subagentId]
					: (params.subagentIds ?? []);
				const activeIds = new Set(
					runtime
						.getActiveSummariesForOwner(callerSessionId)
						.map((agent) => agent.id),
				);
				const missingIds = requestedIds.filter((id) => !activeIds.has(id));

				if (missingIds.length > 0) {
					return toolError(
						`Subagent(s) not active in the current session: ${missingIds.join(", ")}`,
					);
				}

				for (const id of requestedIds) {
					if (runtime.abort(id, { reason: abortReason })) {
						abortedIds.push(id);
					}
				}
			}

			if (abortedIds.length === 0) {
				return toolError("No active subagents to abort in the current session.");
			}

			return toolSuccess(
				`Aborted ${abortedIds.length} subagent(s): ${abortedIds.join(", ")}`,
				{ ids: abortedIds },
				{ terminate: true },
			);
		},
	});
};
