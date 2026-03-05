import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import type { Plugin } from "@opencode-ai/plugin";
import type { Event } from "@opencode-ai/sdk";

import { createLogger, type Logger } from "./common/logger";
import type { OpencodeClient } from "./common/types";
import { getProjectId } from "./common/getProjectId";
import { DelegationManager } from "./background-agents/delegationManager";
import {
	createDelegate,
	createDelegationList,
	createDelegationRead,
} from "./background-agents/delegate";
import { parseAgentWriteCapability } from "./common/parseAgentWriteCapability";
import { parseAgentMode } from "./common/parseAgentMode";
import { injectDelegationRules } from "./background-agents/rules";
import { sessionCompacting } from "./background-agents/sessionCompacting";

const toolExecuteBefore =
	(client: OpencodeClient, log: Logger) =>
	async (
		input: { tool: string },
		output: { args?: { subagent_type?: string } },
	) => {
		// Guard: Only intercept task tool
		if (input.tool !== "task") {
			return;
		}

		// Guard: Require agent name
		const agentName = output.args?.subagent_type;
		if (!agentName) {
			return;
		}

		// Parse boundary 1: Check agent mode
		const { isSubAgent } = await parseAgentMode(
			client as OpencodeClient,
			agentName,
			log,
		);

		// Guard: Allow non-sub-agents (main/built-in)
		if (!isSubAgent) return;

		// Parse boundary 2: Check write capability (only for sub-agents)
		const { isReadOnly } = await parseAgentWriteCapability(
			client as OpencodeClient,
			agentName,
			log,
		);

		// Guard: Allow write-capable agents
		if (!isReadOnly) return;

		// Fail fast: Read-only sub-agent via task is invalid
		throw new Error(
			`❌ Agent '${agentName}' is read-only and should use the delegate tool for async background execution.\n\n` +
				`Read-only agents have: edit="deny", write="deny", bash={"*":"deny"}\n` +
				`Use delegate for: researcher, explore\n` +
				`Use task for: coder, scribe`,
		);
	};

export const backgroundAgentsPlugin: Plugin = async (ctx) => {
	const { client, directory } = ctx;

	// Create logger early for all components
	const log = createLogger(client as OpencodeClient);

	// Project-level storage directory (shared across sessions)
	// Uses git root commit hash for cross-worktree consistency
	const projectId = await getProjectId(directory);
	const baseDir = path.join(
		os.homedir(),
		".local",
		"share",
		"opencode",
		"delegations",
		projectId,
	);

	// Ensure base directory exists (for debug logs etc)
	await fs.mkdir(baseDir, { recursive: true });

	const manager = new DelegationManager(client as OpencodeClient, baseDir, log);

	await manager.debugLog(
		"BackgroundAgentsPlugin initialized with delegation system",
	);

	return {
		tool: {
			delegate: createDelegate(manager),
			delegation_read: createDelegationRead(manager),
			delegation_list: createDelegationList(manager),
		},

		// Prevent read-only agents from using native task tool (symmetric to delegate enforcement)
		"tool.execute.before": toolExecuteBefore(client, log),

		// Inject delegation rules into system prompt
		"experimental.chat.system.transform": injectDelegationRules,

		// Compaction hook - inject delegation context for context recovery
		"experimental.session.compacting": sessionCompacting(manager),

		// Event hook
		event: async ({ event }: { event: Event }): Promise<void> => {
			if (event.type === "session.idle") {
				const sessionID = event.properties.sessionID;
				const delegation = manager.findBySession(sessionID);
				if (delegation) {
					await manager.handleSessionIdle(sessionID);
				}
			}

			if (event.type === "message.updated") {
				const sessionID = event.properties.info.sessionID;
				if (sessionID) {
					manager.handleMessageEvent(sessionID);
				}
			}
		},
	};
};

export default backgroundAgentsPlugin;
