import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { Runtime } from "../runtime";
import Type from "typebox";
import { getToolContext, toolError, toolSuccess } from "./shared";

export const registerTeamRespondTool = (
	pi: ExtensionAPI,
	runtime: Runtime,
): void =>
	pi.registerTool({
		name: "team_respond",
		label: "Respond to team",
		description:
			"Send a follow-up message to a waiting interactive subagent. Returns immediately; the response is delivered as a steering message that starts a new turn.",
		parameters: Type.Object({
			subagentId: Type.String({
				description:
					"ID of the waiting subagent (from team_list or team_spawn result)",
			}),
			message: Type.String({ description: "Message to send to the subagent" }),
		}),
		promptSnippet: "Respond to a waiting interactive subagent.",
		promptGuidelines: [
			"team_respond: Send a complete follow-up only to a waiting interactive subagent.",
			"team_respond: Returns immediately; wait for the next steering result and do not poll.",
		],
		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const toolCtx = getToolContext(ctx);
			const message = params.message.trim();
			if (!message) {
				return toolError("Message is required and must not be empty");
			}

			const { error } = runtime.respond(
				params.subagentId,
				message,
				toolCtx.callerSessionId,
			);
			if (error) {
				return toolError(error);
			}

			return toolSuccess(
				`Message sent to subagent "${params.subagentId}". Response will be delivered as a steering message.`,
				{ id: params.subagentId, message },
			);
		},
	});
