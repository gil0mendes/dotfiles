import type {
	AgentToolResult,
	ExtensionContext,
} from "@earendil-works/pi-coding-agent";

export type ToolContext = {
	cwd: string;
	callerSessionId: string;
};

export const getToolContext = (ctx: ExtensionContext): ToolContext => {
	return {
		cwd: ctx.cwd,
		callerSessionId: ctx.sessionManager.getSessionId(),
	};
};

export type ToolResult = AgentToolResult<unknown> & {
	isError?: boolean;
	terminate?: boolean;
};

export const toolSuccess = (
	text: string,
	details: Record<string, unknown> = {},
	options: { terminate?: boolean } = {},
): ToolResult => {
	return {
		content: [{ type: "text", text }],
		details,
		...(options.terminate ? { terminate: true } : {}),
	};
};

export const toolError = (text: string): ToolResult => {
	return {
		content: [{ type: "text", text }],
		isError: true,
		details: { error: true },
	};
};
