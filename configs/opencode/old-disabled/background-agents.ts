import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { Plugin } from "@opencode/plugin";

import { createLogger } from "./common/logger";
import { getProjectId } from "./common/getProjectId";
import { DelegationManager } from "./background-agents/delegationManager";
import { registerDelegationTools } from "./background-agents/delegate";
import { parseAgentWriteCapability } from "./common/parseAgentWriteCapability";
import { injectDelegationRules } from "./background-agents/rules";
import { formatDelegationContext } from "./background-agents/sessionCompacting";

function delegationAgent(input: unknown): string | undefined {
	if (typeof input !== "object" || input === null) return undefined;
	if (!("subagent_type" in input)) return undefined;
	return typeof input.subagent_type === "string" ? input.subagent_type : undefined;
}

export default Plugin.define({
	id: "background-agents",
	async setup(ctx) {
		const log = createLogger();
		const projectId = await getProjectId(ctx.location.directory);
		const baseDir = path.join(os.homedir(), ".local", "share", "opencode", "delegations", projectId);
		await fs.mkdir(baseDir, { recursive: true });
		const manager = new DelegationManager(ctx, baseDir, log);
		await registerDelegationTools(ctx, manager);

		await ctx.tool.hook("execute.before", async (event) => {
			if (event.tool !== "task") return;
			const agent = delegationAgent(event.input);
			if (!agent) return;
			const { isReadOnly } = await parseAgentWriteCapability(ctx, agent, log);
			if (!isReadOnly) return;
			throw new Error(`Agent '${agent}' is read-only. Use delegate for async background execution.`);
		});

		await ctx.session.hook("context", (event) => {
			injectDelegationRules(event.system);
		});

		await ctx.session.hook("prompt", (event) => {
			const notifications = manager.takePendingNotifications(event.sessionID);
			if (notifications.length === 0) return;
			event.prompt.text = `${notifications.join("\n\n")}\n\n${event.prompt.text}`;
		});

		await ctx.session.hook("compaction", async (event) => {
			const rootSessionID = await manager.getRootSessionID(event.sessionID);
			const running = manager.getRunningDelegations(rootSessionID);
			const completed = manager.getUnreadCompletedDelegations(rootSessionID);
			if (running.length === 0 && completed.length === 0) return;
			event.system.push({ type: "text", text: formatDelegationContext(running, completed) });
		});

		const controller = new AbortController();
		void (async () => {
			for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
				if (event.type === "session.idle") await manager.handleSessionIdle(event.data.sessionID);
			}
		})().catch((error: unknown) => {
			if (!controller.signal.aborted) log.warn(`event subscription failed: ${error instanceof Error ? error.message : String(error)}`);
		});

		return () => controller.abort();
	},
});
