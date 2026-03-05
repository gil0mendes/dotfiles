import * as path from "node:path";
import * as fs from "node:fs/promises";

import type { OpencodeClient } from "../common/types";
import type { Logger } from "../common/logger";
import { generateReadableId } from "../common/generateReadableId";
import { parseAgentWriteCapability } from "../common/parseAgentWriteCapability";
import { generateMetadata } from "../common/generateMetadata";
import type { Message, Part, TextPart } from "@opencode-ai/sdk";

const MAX_RUN_TIME_MS = 15 * 60 * 1000; // 15 minutes

export type DelegationProgress = {
	toolCalls: number;
	lastUpdate: Date;
	lastMessage?: string;
	lastMessageAt?: Date;
};

type Delegation = {
	id: string; // Human-readable ID (e.g., "swift-amber-falcon")
	sessionID: string;
	parentSessionID: string;
	parentMessageID: string;
	parentAgent: string;
	prompt: string;
	agent: string;
	status: "running" | "complete" | "error" | "cancelled" | "timeout";
	startedAt: Date;
	completedAt?: Date;
	progress: DelegationProgress;
	error?: string;
	// Generated on completion by small_model
	title?: string;
	description?: string;
	result?: string;
};

type DelegateInput = {
	parentSessionID: string;
	parentMessageID: string;
	parentAgent: string;
	prompt: string;
	agent: string;
};

type DelegationListItem = {
	id: string;
	status: string;
	title?: string;
	description?: string;
	agent?: string;
};

type SessionMessageItem = {
	info: Message;
	parts: Part[];
};

type AssistantSessionMessageItem = {
	info: Message & { role: "assistant" };
	parts: Part[];
};

export class DelegationManager {
	private delegations: Map<string, Delegation> = new Map();
	private client: OpencodeClient;
	private baseDir: string;
	private log: Logger;
	// Track pending delegations per parent session for batched notifications
	private pendingByParent: Map<string, Set<string>> = new Map();

	constructor(client: OpencodeClient, baseDir: string, log: Logger) {
		this.client = client;
		this.baseDir = baseDir;
		this.log = log;
	}

	/**
	 * Resolves the root session ID by walking up the parent chain.
	 */
	async getRootSessionID(sessionID: string): Promise<string> {
		let currentID = sessionID;
		// Prevent infinite loops with max depth
		for (let depth = 0; depth < 10; depth++) {
			try {
				const session = await this.client.session.get({
					path: { id: currentID },
				});

				if (!session.data?.parentID) {
					return currentID;
				}

				currentID = session.data.parentID;
			} catch {
				// If we can't fetch the session, assume current is root or best effort
				return currentID;
			}
		}
		return currentID;
	}

	/**
	 * Get the delegations directory for a session scope (root session)
	 */
	private async getDelegationsDir(sessionID: string): Promise<string> {
		const rootID = await this.getRootSessionID(sessionID);
		return path.join(this.baseDir, rootID);
	}

	/**
	 * Ensure the delegations directory exists
	 */
	private async ensureDelegationsDir(sessionID: string): Promise<string> {
		const dir = await this.getDelegationsDir(sessionID);
		await fs.mkdir(dir, { recursive: true });
		return dir;
	}

	/**
	 * Delegate a task to an agent
	 */
	async delegate(input: DelegateInput): Promise<Delegation> {
		// Generate readable ID
		const id = generateReadableId();
		await this.debugLog(`delegate() called, generated ID: ${id}`);

		// Check for ID collisions (regenerate if needed)
		let finalId = id;
		let attempts = 0;
		while (this.delegations.has(finalId) && attempts < 10) {
			finalId = generateReadableId();
			attempts++;
		}
		if (this.delegations.has(finalId)) {
			throw new Error(
				"Failed to generate unique delegation ID after 10 attempts",
			);
		}

		// Validate agent exists before creating session
		const agentsResult = await this.client.app.agents({});
		const agents = (agentsResult.data ?? []) as {
			name: string;
			description?: string;
			mode?: string;
		}[];
		const validAgent = agents.find((a) => a.name === input.agent);

		if (!validAgent) {
			const available = agents
				.filter((a) => a.mode === "subagent" || a.mode === "all" || !a.mode)
				.map((a) => `• ${a.name}${a.description ? ` - ${a.description}` : ""}`)
				.join("\n");

			throw new Error(
				`Agent "${input.agent}" not found.\n\nAvailable agents:\n${available || "(none)"}`,
			);
		}

		// Check if agent is read-only (Early Exit + Fail Fast)
		const { isReadOnly } = await parseAgentWriteCapability(
			this.client,
			input.agent,
			this.log,
		);
		if (!isReadOnly) {
			throw new Error(
				`Agent "${input.agent}" is write-capable and requires the native \`task\` tool for proper undo/branching support.\n\n` +
					`Use \`task\` instead of \`delegate\` for write-capable agents.\n\n` +
					`Read-only agents (researcher, explore) use \`delegate\`.\n` +
					`Write-capable agents (coder, scribe) use \`task\`.`,
			);
		}

		// Create isolated session for delegation
		const sessionResult = await this.client.session.create({
			body: {
				title: `Delegation: ${finalId}`,
				parentID: input.parentSessionID,
			},
		});

		await this.debugLog(
			`session.create result: ${JSON.stringify(sessionResult.data)}`,
		);

		if (!sessionResult.data?.id) {
			throw new Error("Failed to create delegation session");
		}

		const delegation: Delegation = {
			id: finalId,
			sessionID: sessionResult.data.id,
			parentSessionID: input.parentSessionID,
			parentMessageID: input.parentMessageID,
			parentAgent: input.parentAgent,
			prompt: input.prompt,
			agent: input.agent,
			status: "running",
			startedAt: new Date(),
			progress: {
				toolCalls: 0,
				lastUpdate: new Date(),
			},
		};

		await this.debugLog(`Created delegation ${delegation.id}`);
		this.delegations.set(delegation.id, delegation);

		// Track this delegation for batched notification
		const parentId = input.parentSessionID;
		if (!this.pendingByParent.has(parentId)) {
			this.pendingByParent.set(parentId, new Set());
		}
		this.pendingByParent.get(parentId)?.add(delegation.id);
		await this.debugLog(
			`Tracking delegation ${delegation.id} for parent ${parentId}. Pending count: ${this.pendingByParent.get(parentId)?.size}`,
		);

		await this.debugLog(
			`Delegation added to map. Current delegations: ${Array.from(this.delegations.keys()).join(", ")}`,
		);

		// Set a timer for the global max run time
		setTimeout(() => {
			const current = this.delegations.get(delegation.id);
			if (current && current.status === "running") {
				this.handleTimeout(delegation.id);
			}
		}, MAX_RUN_TIME_MS + 5000); // Adding 5s buffer

		// Ensure delegations directory exists (early check)
		await this.ensureDelegationsDir(input.parentSessionID);

		// Fire the prompt (using prompt() instead of promptAsync() to properly initialize agent loop)
		// Agent param is critical for MCP tools - tells OpenCode which agent's config to use
		// Anti-recursion: disable nested delegations and state-modifying tools via tools config
		this.client.session
			.prompt({
				path: { id: delegation.sessionID },
				body: {
					agent: input.agent,
					parts: [{ type: "text", text: input.prompt }],
					tools: {
						task: false,
						delegate: false,
						todowrite: false,
						plan_save: false,
					},
				},
			})
			.catch((error: Error) => {
				delegation.status = "error";
				delegation.error = error.message;
				delegation.completedAt = new Date();
				this.persistOutput(delegation, `Error: ${error.message}`);
				this.notifyParent(delegation);
			});

		return delegation;
	}

	/**
	 * Handle delegation timeout
	 */
	private async handleTimeout(delegationId: string): Promise<void> {
		const delegation = this.delegations.get(delegationId);
		if (!delegation || delegation.status !== "running") return;

		await this.debugLog(`handleTimeout for delegation ${delegation.id}`);

		delegation.status = "timeout";
		delegation.completedAt = new Date();
		delegation.error = `Delegation timed out after ${MAX_RUN_TIME_MS / 1000}s`;

		// Try to cancel the session
		try {
			await this.client.session.delete({
				path: { id: delegation.sessionID },
			});
		} catch {
			// Ignore
		}

		// Get whatever result was produced so far
		const result = await this.getResult(delegation);
		await this.persistOutput(delegation, `${result}\n\n[TIMEOUT REACHED]`);

		// Notify parent session
		await this.notifyParent(delegation);
	}

	/**
	 * Wait for a delegation to complete (polling)
	 */
	private async waitForCompletion(delegationId: string): Promise<void> {
		const pollInterval = 1000;
		const startTime = Date.now();

		const delegation = this.delegations.get(delegationId);
		if (!delegation) return;

		while (
			delegation.status === "running" &&
			Date.now() - startTime < MAX_RUN_TIME_MS + 10000 // Slightly more than global limit
		) {
			await new Promise((resolve) => setTimeout(resolve, pollInterval));
		}
	}

	/**
	 * Handle session.idle event - called when a session becomes idle
	 */
	async handleSessionIdle(sessionID: string): Promise<void> {
		const delegation = this.findBySession(sessionID);
		if (!delegation || delegation.status !== "running") return;

		await this.debugLog(`handleSessionIdle for delegation ${delegation.id}`);

		delegation.status = "complete";
		delegation.completedAt = new Date();

		// Get the result
		const result = await this.getResult(delegation);
		delegation.result = result;

		// Generate title and description using small model
		const metadata = await generateMetadata(
			this.client,
			result,
			delegation.sessionID,
			(msg) => this.debugLog(msg),
		);
		delegation.title = metadata.title;
		delegation.description = metadata.description;

		// Persist output with generated metadata
		await this.persistOutput(delegation, result);

		// Notify parent session
		await this.notifyParent(delegation);
	}

	/**
	 * Get the result from a delegation's session
	 */
	private async getResult(delegation: Delegation): Promise<string> {
		try {
			const messages = await this.client.session.messages({
				path: { id: delegation.sessionID },
			});

			const messageData = messages.data as SessionMessageItem[] | undefined;

			if (!messageData || messageData.length === 0) {
				await this.debugLog(
					`getResult: No messages found for session ${delegation.sessionID}`,
				);
				return `Delegation "${delegation.description}" completed but produced no output.`;
			}

			await this.debugLog(
				`getResult: Found ${messageData.length} messages. Roles: ${messageData.map((m) => m.info.role).join(", ")}`,
			);

			// Find the last message from the assistant/model
			const isAssistantMessage = (
				m: SessionMessageItem,
			): m is AssistantSessionMessageItem => m.info.role === "assistant";

			const assistantMessages = messageData.filter(isAssistantMessage);

			if (assistantMessages.length === 0) {
				await this.debugLog(
					`getResult: No assistant messages found in ${JSON.stringify(messageData.map((m) => ({ role: m.info.role, keys: Object.keys(m) })))}`,
				);
				return `Delegation "${delegation.description}" completed but produced no assistant response.`;
			}

			const lastMessage = assistantMessages[assistantMessages.length - 1];

			// Extract text parts from the message
			const isTextPart = (p: Part): p is TextPart => p.type === "text";
			const textParts = lastMessage?.parts.filter(isTextPart) ?? [];

			if (textParts.length === 0) {
				await this.debugLog(
					`getResult: No text parts found in message: ${JSON.stringify(lastMessage)}`,
				);
				return `Delegation "${delegation.description}" completed but produced no text content.`;
			}

			return textParts.map((p) => p.text).join("\n");
		} catch (error) {
			await this.debugLog(
				`getResult error: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
			return `Delegation "${delegation.description}" completed but result could not be retrieved: ${
				error instanceof Error ? error.message : "Unknown error"
			}`;
		}
	}

	/**
	 * Persist delegation output to storage
	 */
	private async persistOutput(
		delegation: Delegation,
		content: string,
	): Promise<void> {
		try {
			// Ensure we resolve the root session ID of the PARENT session for storage
			const dir = await this.ensureDelegationsDir(delegation.parentSessionID);
			const filePath = path.join(dir, `${delegation.id}.md`);

			// Use title/description if available (generated by small model), otherwise fallback
			const title = delegation.title || delegation.id;
			const description =
				delegation.description || "(No description generated)";

			const header = `# ${title}

${description}

**ID:** ${delegation.id}
**Agent:** ${delegation.agent}
**Status:** ${delegation.status}
**Started:** ${delegation.startedAt.toISOString()}
**Completed:** ${delegation.completedAt?.toISOString() || "N/A"}

---

`;
			await fs.writeFile(filePath, header + content, "utf8");
			await this.debugLog(`Persisted output to ${filePath}`);
		} catch (error) {
			await this.debugLog(
				`Failed to persist output: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
		}
	}

	/**
	 * Notify parent session that delegation is complete.
	 * Uses batching: individual notifications are silent (noReply: true),
	 * but when ALL delegations for a parent session complete, triggers a response.
	 */
	private async notifyParent(delegation: Delegation): Promise<void> {
		try {
			// Use generated title/description if available
			const title = delegation.title || delegation.id;
			const statusText =
				delegation.status === "complete" ? "complete" : delegation.status;
			const result = delegation.result || "(No result)";

			// Mark this delegation as complete in the pending tracker
			const pendingSet = this.pendingByParent.get(delegation.parentSessionID);
			if (pendingSet) {
				pendingSet.delete(delegation.id);
			}

			// Check if ALL delegations for this parent are now complete
			const allComplete = !pendingSet || pendingSet.size === 0;

			// Clean up if all complete
			if (allComplete && pendingSet) {
				this.pendingByParent.delete(delegation.parentSessionID);
			}

			const remainingCount = pendingSet?.size || 0;

			// Always send the completed delegation notification first
			const progressNote =
				remainingCount > 0
					? `
**${remainingCount} delegation${remainingCount === 1 ? "" : "s"} still in progress.** You WILL be notified when ALL complete.
❌ Do NOT poll \`delegation_list\` - continue productive work.`
					: "";
			const completionNotification = `<task-notification>
<task-id>${delegation.id}</task-id>
<status>${statusText}</status>
<summary>Agent "${title}" ${statusText}</summary>
<result>${result}</result>
${delegation.error ? `\n<error>${delegation.error}</error>` : ""}
</task-notification>${progressNote}`;

			await this.client.session.prompt({
				path: { id: delegation.parentSessionID },
				body: {
					noReply: true,
					agent: delegation.parentAgent,
					parts: [{ type: "text", text: completionNotification }],
				},
			});

			// If all delegations complete, send a minimal completion notice that triggers response
			if (allComplete) {
				const allCompleteNotification = `<task-notification>
<status>completed</status>
<summary>All delegations complete.</summary>
</task-notification>`;

				await this.client.session.prompt({
					path: { id: delegation.parentSessionID },
					body: {
						noReply: false,
						agent: delegation.parentAgent,
						parts: [{ type: "text", text: allCompleteNotification }],
					},
				});
			}

			await this.debugLog(
				`Notified parent session ${delegation.parentSessionID} (allComplete=${allComplete}, remaining=${pendingSet?.size || 0})`,
			);
		} catch (error) {
			await this.debugLog(
				`Failed to notify parent: ${error instanceof Error ? error.message : "Unknown error"}`,
			);
		}
	}

	/**
	 * Read a delegation's output by ID. Blocks if the delegation is still running.
	 */
	async readOutput(sessionID: string, id: string): Promise<string> {
		// Try to find the file
		let filePath: string | undefined;
		try {
			const dir = await this.getDelegationsDir(sessionID);
			filePath = path.join(dir, `${id}.md`);
			// Check if file exists
			await fs.access(filePath);
			return await fs.readFile(filePath, "utf8");
		} catch {
			// File doesn't exist yet, continue to check memory
		}

		// Check if it's currently running in memory
		const delegation = this.delegations.get(id);
		if (delegation) {
			if (delegation.status === "running") {
				await this.debugLog(
					`readOutput: waiting for delegation ${delegation.id} to complete`,
				);
				await this.waitForCompletion(delegation.id);

				// Re-check after waiting
				const dir = await this.getDelegationsDir(sessionID);
				filePath = path.join(dir, `${id}.md`);
				try {
					return await fs.readFile(filePath, "utf8");
				} catch {
					// Still failed to read
				}

				// If still no file after waiting (e.g. error/timeout/cancel)
				const updated = this.delegations.get(id);
				if (updated && updated.status !== "running") {
					const title = updated.title || updated.id;
					return `Delegation "${title}" ended with status: ${updated.status}. ${updated.error || ""}`;
				}
			}
		}

		throw new Error(
			`Delegation "${id}" not found.\n\nUse delegation_list() to see available delegations.`,
		);
	}

	/**
	 * List all delegations for a session
	 */
	async listDelegations(sessionID: string): Promise<DelegationListItem[]> {
		const results: DelegationListItem[] = [];

		// Add in-memory delegations that match this session (or parent)
		for (const delegation of this.delegations.values()) {
			results.push({
				id: delegation.id,
				status: delegation.status,
				title: delegation.title || "(generating...)",
				description: delegation.description || "(generating...)",
			});
		}

		// Check filesystem for persisted delegations
		try {
			const dir = await this.getDelegationsDir(sessionID);
			const files = await fs.readdir(dir);

			for (const file of files) {
				if (file.endsWith(".md")) {
					const id = file.replace(".md", "");
					// Deduplicate: prioritize in-memory status
					if (!results.find((r) => r.id === id)) {
						// Try to read title, agent, description from file
						let title = "(loaded from storage)";
						let description = "";
						let agent: string | undefined;
						try {
							const filePath = path.join(dir, file);
							const content = await fs.readFile(filePath, "utf8");
							const titleMatch = content.match(/^# (.+)$/m);
							if (titleMatch) {
								title = titleMatch[1];
							}
							const agentMatch = content.match(/^\*\*Agent:\*\* (.+)$/m);
							if (agentMatch) agent = agentMatch[1];
							// Get first paragraph after title as description
							const lines = content.split("\n");
							if (lines.length > 2 && lines[2]) {
								description = lines[2].slice(0, 150);
							}
						} catch {
							// Ignore read errors
						}
						results.push({
							id,
							status: "complete",
							title,
							description,
							agent,
						});
					}
				}
			}
		} catch {
			// Directory may not exist yet
		}

		return results;
	}

	/**
	 * Delete a delegation by id (cancels if running, removes from storage)
	 * Used internally for cleanup (timeout, etc.)
	 */
	async deleteDelegation(sessionID: string, id: string): Promise<boolean> {
		// Find delegation by id
		let delegationId: string | undefined;
		for (const [dId, d] of this.delegations) {
			if (d.id === id) {
				delegationId = dId;
				break;
			}
		}

		if (delegationId) {
			const delegation = this.delegations.get(delegationId);
			if (delegation?.status === "running") {
				try {
					await this.client.session.delete({
						path: { id: delegation.sessionID },
					});
				} catch {
					// Session may already be deleted
				}
				delegation.status = "cancelled";
				delegation.completedAt = new Date();
			}
			this.delegations.delete(delegationId);
		}

		// Remove from filesystem
		try {
			const dir = await this.getDelegationsDir(sessionID);
			const filePath = path.join(dir, `${id}.md`);
			await fs.unlink(filePath);
			return true;
		} catch {
			return false;
		}
	}

	/**
	 * Find a delegation by its session ID
	 */
	findBySession(sessionID: string): Delegation | undefined {
		return Array.from(this.delegations.values()).find(
			(d) => d.sessionID === sessionID,
		);
	}

	/**
	 * Handle message events for progress tracking
	 */
	handleMessageEvent(sessionID: string, messageText?: string): void {
		const delegation = this.findBySession(sessionID);
		if (!delegation || delegation.status !== "running") return;

		delegation.progress.lastUpdate = new Date();
		if (messageText) {
			delegation.progress.lastMessage = messageText;
			delegation.progress.lastMessageAt = new Date();
		}
	}

	/**
	 * Get count of pending delegations for a parent session
	 */
	getPendingCount(parentSessionID: string): number {
		const pendingSet = this.pendingByParent.get(parentSessionID);
		return pendingSet ? pendingSet.size : 0;
	}

	/**
	 * Get all currently running delegations (in-memory only)
	 */
	getRunningDelegations(): Delegation[] {
		return Array.from(this.delegations.values()).filter(
			(d) => d.status === "running",
		);
	}

	/**
	 * Get recent completed delegations for compaction injection
	 */
	async getRecentCompletedDelegations(
		sessionID: string,
		limit: number = 10,
	): Promise<DelegationListItem[]> {
		const all = await this.listDelegations(sessionID);
		return all.filter((d) => d.status !== "running").slice(-limit);
	}

	/**
	 * Log debug messages
	 */
	async debugLog(msg: string): Promise<void> {
		// Only log if debug is enabled (could be env var or static const)
		// For now, mirroring previous behavior but writing to the new baseDir/debug.log
		const timestamp = new Date().toISOString();
		const line = `${timestamp}: ${msg}\n`;
		const debugFile = path.join(this.baseDir, "background-agents-debug.log");

		try {
			await fs.appendFile(debugFile, line, "utf8");
		} catch {
			// Ignore errors, try to ensure dir once if it fails?
			// Simpler to just ignore for debug logs
		}
	}
}
