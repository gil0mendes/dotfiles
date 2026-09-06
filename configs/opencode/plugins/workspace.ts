import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { Plugin } from "@opencode/plugin";
import { z } from "zod";
import { getProjectId } from "./common/getProjectId";
import { ruleInjection } from "./workspace/rules";

// ==========================================
// PLAN SCHEMA & VALIDATION
// ==========================================

const PhaseStatus = z.enum(["PENDING", "IN PROGRESS", "COMPLETE", "BLOCKED"]);

const TaskSchema = z.object({
	id: z
		.string()
		.regex(/^\d+\.\d+$/, "Task ID must be hierarchical (e.g., '2.1')"),
	checked: z.boolean(),
	content: z.string().min(1, "Task content cannot be empty"),
	isCurrent: z.boolean().optional(),
	citation: z
		.string()
		.regex(
			/^ref:[a-z]+-[a-z]+-[a-z]+$/,
			"Citation must be ref:word-word-word format",
		)
		.optional(),
});

const PhaseSchema = z.object({
	number: z.number().int().positive(),
	name: z.string().min(1, "Phase name cannot be empty"),
	status: PhaseStatus,
	tasks: z.array(TaskSchema).min(1, "Phase must have at least one task"),
});

const FrontmatterSchema = z.object({
	status: z.enum(["not-started", "in-progress", "complete", "blocked"]),
	phase: z.number().int().positive(),
	updated: z.string().regex(/^\d{4}-\d{2}-\d{2}$/, "Date must be YYYY-MM-DD"),
});

const PlanSchema = z.object({
	frontmatter: FrontmatterSchema,
	goal: z.string().min(10, "Goal must be at least 10 characters"),
	context: z
		.array(
			z.object({
				decision: z.string(),
				rationale: z.string(),
				source: z.string(),
			}),
		)
		.optional(),
	phases: z.array(PhaseSchema).min(1, "Plan must have at least one phase"),
});

/**
 * Result type for plan parsing - either valid data or descriptive error.
 * Follows Law 2: Parse Don't Validate - boundary parsing returns trusted types.
 */
type ParseResult =
	| { ok: true; data: z.infer<typeof PlanSchema>; warnings: string[] }
	| { ok: false; error: string; hint: string };

/**
 * Raw extracted parts from markdown (no validation).
 * Used as intermediate type before Zod validation.
 */
interface ExtractedParts {
	frontmatter: Record<string, string | number> | null;
	goal: string | null;
	phases: Array<{
		number: number;
		name: string;
		status: string;
		tasks: Array<{
			id: string;
			checked: boolean;
			content: string;
			isCurrent: boolean;
			citation?: string;
		}>;
	}>;
}

/**
 * Extract all parts from markdown without validation (Law 2: Parse Don't Validate).
 * Returns raw extracted data - validation happens in parsePlanMarkdown.
 * This is a pure extraction function (Law 3: Purity).
 */
function extractMarkdownParts(content: string): ExtractedParts {
	// Extract frontmatter (no validation - just extraction)
	const fmMatch = content.match(/^---\n([\s\S]*?)\n---/);
	let frontmatter: Record<string, string | number> | null = null;

	const frontmatterText = fmMatch?.[1];
	if (frontmatterText) {
		frontmatter = {};
		const fmLines = frontmatterText.split("\n");
		for (const line of fmLines) {
			const [key, ...valueParts] = line.split(":");
			if (key && valueParts.length > 0) {
				const value = valueParts.join(":").trim();
				frontmatter[key.trim()] =
					key.trim() === "phase" ? parseInt(value, 10) : value;
			}
		}
	}

	// Extract goal section body (supports blank line + multi-line text)
	const goalMatch = content.match(
		/## Goal\s*\n([\s\S]*?)(?=\n## |\n# |\n---|$)/,
	);
	const goal = goalMatch?.[1]?.trim() || null;

	// Extract phases (no validation - just extraction)
	const phases: ExtractedParts["phases"] = [];
	const phaseRegex =
		/## Phase (\d+): ([^[]+)\[([^\]]+)\]\n([\s\S]*?)(?=## Phase \d+:|## Notes|## Blockers|$)/g;

	let phaseMatch = phaseRegex.exec(content);
	while (phaseMatch !== null) {
		const [, phaseNumberText, phaseNameText, phaseStatusText, phaseContent] = phaseMatch;
		if (!phaseNumberText || !phaseNameText || !phaseStatusText || !phaseContent) {
			phaseMatch = phaseRegex.exec(content);
			continue;
		}
		const phaseNum = parseInt(phaseNumberText, 10);
		const phaseName = phaseNameText.trim();
		const phaseStatus = phaseStatusText.trim();

		const tasks: ExtractedParts["phases"][0]["tasks"] = [];
		const taskRegex =
			/- \[([ x])\] (\*\*)?(\d+\.\d+) ([^←\n]+)(← CURRENT)?.*?(`ref:[a-z]+-[a-z]+-[a-z]+`)?/g;

		let taskMatch = taskRegex.exec(phaseContent);
		while (taskMatch !== null) {
			const [, checkedText, , taskID, taskContent, currentMarker, citation] = taskMatch;
			if (!checkedText || !taskID || !taskContent) {
				taskMatch = taskRegex.exec(phaseContent);
				continue;
			}
			tasks.push({
				id: taskID,
				checked: checkedText === "x",
				content: taskContent.trim().replace(/\*\*/g, ""),
				isCurrent: currentMarker !== undefined,
				citation: citation?.replace(/`/g, ""),
			});
			taskMatch = taskRegex.exec(phaseContent);
		}

		// Include phase even if no tasks (let Zod validate)
		phases.push({
			number: phaseNum,
			name: phaseName,
			status: phaseStatus,
			tasks,
		});
		phaseMatch = phaseRegex.exec(content);
	}

	return { frontmatter, goal, phases };
}

/**
 * Format Zod validation errors into human-readable messages (Law 4: Fail Loud).
 * Shows ALL errors at once with clear paths.
 */
function formatZodErrors(error: z.ZodError): string {
	const errorMessages: string[] = [];

	for (const issue of error.issues) {
		const path = issue.path.length > 0 ? `[${issue.path.join(".")}]` : "[root]";

		// Provide helpful context based on error type
		let message = issue.message;
		if (issue.code === "invalid_value") {
			const values = (issue as { values?: unknown[] }).values;
			const input = (issue as { input?: unknown }).input;
			message = `Invalid value "${input}". Expected: ${values?.join(" | ") ?? "valid value"}`;
		} else if (
			issue.code === "invalid_type" &&
			(issue as { input?: unknown }).input === null
		) {
			message = "Required field missing";
		}

		errorMessages.push(`${path}: ${message}`);
	}

	return errorMessages.join("\n");
}

/**
 * Parse and validate markdown plan in a single boundary operation.
 * Returns ParseResult: either trusted data or descriptive error with hint.
 *
 * Follows all 5 Laws:
 * - Law 1 (Early Exit): Guard at top for empty content
 * - Law 2 (Parse Don't Validate): Extract all → validate once at end
 * - Law 3 (Purity): No side effects, same input = same output
 * - Law 4 (Fail Loud): Shows ALL validation errors with clear paths
 * - Law 5 (Intentional Naming): Self-documenting function names
 */
function parsePlanMarkdown(content: string): ParseResult {
	const skillHint = "Load skill('plan-protocol') for the full format spec.";

	// Guard: Content must be string (Law 1: Early Exit, Law 2: Parse at boundary)
	if (typeof content !== "string") {
		return {
			ok: false,
			error: `Expected markdown string, received ${typeof content}`,
			hint: skillHint,
		};
	}

	// Guard: Empty content (Law 1: Early Exit)
	if (!content.trim()) {
		return {
			ok: false,
			error: "Empty content provided",
			hint: skillHint,
		};
	}

	// Extract all parts without validation (Law 2: Parse Don't Validate)
	const parts = extractMarkdownParts(content);

	// Build candidate object for validation
	const candidate = {
		frontmatter: parts.frontmatter,
		goal: parts.goal,
		phases: parts.phases,
	};

	// Single validation point: Zod schema (Law 2: Parse Don't Validate)
	const result = PlanSchema.safeParse(candidate);
	if (!result.success) {
		return {
			ok: false,
			error: formatZodErrors(result.error),
			hint: skillHint,
		};
	}

	// Business rules validation (still part of single boundary)
	const warnings: string[] = [];
	let currentCount = 0;
	let inProgressCount = 0;

	for (const phase of result.data.phases) {
		if (phase.status === "IN PROGRESS") inProgressCount++;
		for (const task of phase.tasks) {
			if (task.isCurrent) currentCount++;
		}
	}

	if (currentCount > 1) {
		return {
			ok: false,
			error: `Multiple tasks marked ← CURRENT (found ${currentCount}). Only one task may be current.`,
			hint: skillHint,
		};
	}

	if (inProgressCount > 1) {
		warnings.push(
			"Multiple phases marked IN PROGRESS. Consider focusing on one phase at a time.",
		);
	}

	return { ok: true, data: result.data, warnings };
}

/**
 * Format parse error with actionable guidance (Law 4: Fail Loud).
 * Includes error message, example, and skill hint.
 */
function formatParseError(error: string, hint: string): string {
	return `❌ Plan validation failed:

${error}

💡 ${hint}`;
}

/**
 * Type guard for Node.js filesystem errors (ENOENT, EACCES, etc.)
 * Follows "Parse, Don't Validate" - handle uncertainty at boundaries.
 */
function isNodeError(error: unknown): error is NodeJS.ErrnoException {
	return error instanceof Error && "code" in error;
}

function stringInput(input: unknown, key: string): string | undefined {
	if (typeof input !== "object" || input === null) return undefined;
	const value = Object.fromEntries(Object.entries(input))[key];
	return typeof value === "string" ? value : undefined;
}

/**
 * KDCO Workspace Plugin
 *
 * Provides plan management and targeted rule injection.
 * Research functionality has been moved to the delegation system (background-agents).
 * Follows "Elegant Defense" philosophy: Flat, Safe, and Fast.
 */

// ==========================================
// CODER TASK TRACKING FOR REVIEW TRIGGER
// ==========================================

/** Tracks in-flight coder task callIDs with timestamps for stale cleanup */
const activeCoderCalls = new Map<string, { startTime: number }>();

/** Stale call timeout - matches MAX_RUN_TIME_MS in background-agents.ts */
const STALE_CALL_TIMEOUT_MS = 15 * 60 * 1000;

/** Periodic cleanup of orphaned callIDs (runs every 60s) */
const cleanupInterval = setInterval(() => {
	const now = Date.now();
	for (const [callID, data] of activeCoderCalls) {
		if (now - data.startTime > STALE_CALL_TIMEOUT_MS) {
			activeCoderCalls.delete(callID);
		}
	}
}, 60_000);
// Prevent interval from keeping process alive
cleanupInterval.unref?.();

// ==========================================
// RULES FOR INJECTION
// ==========================================

export default Plugin.define({
	id: "workspace",
	async setup(ctx) {
	const directory = ctx.location.directory;

	// Use git root commit hash for cross-worktree consistency
	const projectId = await getProjectId(directory);
	const baseDir = path.join(
		os.homedir(),
		".local",
		"share",
		"opencode",
		"workspace",
		projectId,
	);

	/**
	 * Resolves the root session ID by walking up the parent chain.
	 */
	async function getRootSessionID(sessionID?: string): Promise<string> {
		if (!sessionID) {
			throw new Error("sessionID is required to resolve root session scope");
		}

		let currentID = sessionID;
		for (let depth = 0; depth < 10; depth++) {
			const session = await ctx.session.get({ sessionID: currentID });

			if (!session.parentID) {
				return currentID;
			}

			currentID = session.parentID;
		}

		throw new Error(
			"Failed to resolve root session: maximum traversal depth exceeded",
		);
	}

	await ctx.tool.transform((editor) => {
		editor.add({
				name: "plan_save",
				description:
					"Save the implementation plan as markdown. Must include citations (ref:delegation-id) for decisions based on research. Plan is validated before saving.",
				input: {
					type: "object",
					properties: { content: { type: "string", description: "The full plan in markdown format" } },
					required: ["content"],
					additionalProperties: false,
				},
				async execute(input, toolContext) {
					// Guard 1: Session required (Law 1: Early Exit)
					const content = stringInput(input, "content");
					if (content === undefined) {
						return { content: "❌ plan_save requires string content." };
					}

					const rootID = await getRootSessionID(toolContext.sessionID);
					const sessionDir = path.join(baseDir, rootID);
					await fs.mkdir(sessionDir, { recursive: true });

					// Guard 2: Parse and validate at boundary (Law 2: Parse Don't Validate)
					const result = parsePlanMarkdown(content);
					if (!result.ok) {
						return { content: formatParseError(result.error, result.hint) };
					}

					// Happy path: save
					await fs.writeFile(
						path.join(sessionDir, "plan.md"),
						content,
						"utf8",
					);
					const warningCount = result.warnings?.length ?? 0;
					const warningText =
						warningCount > 0
							? ` (${warningCount} warnings: ${result.warnings?.join(", ")})`
							: "";

					return { content: `Plan saved.${warningText}` };
				},
			});
			editor.add({
				name: "plan_read",
				description: "Read the current implementation plan for this session.",
				input: {
					type: "object",
					properties: { reason: { type: "string", description: "Brief explanation of why you are calling this tool" } },
					required: ["reason"],
					additionalProperties: false,
				},
				async execute(_input, toolContext) {
					// Guard: Session required (Law 1: Early Exit)
					const rootID = await getRootSessionID(toolContext.sessionID);
					const planPath = path.join(baseDir, rootID, "plan.md");
					try {
						return { content: await fs.readFile(planPath, "utf8") };
					} catch (error) {
						if (isNodeError(error) && error.code === "ENOENT")
							return { content: "No plan found." };
						throw error;
					}
				},
			});
	});

	await ctx.session.hook("context", (event) => {
		ruleInjection(event.agent, event.system);
	});

	await ctx.tool.hook("execute.before", (event) => {
		if (event.tool !== "task") return;
		if (typeof event.input !== "object" || event.input === null) return;
		if (!("subagent_type" in event.input) || event.input.subagent_type !== "coder") return;
		activeCoderCalls.set(event.id, { startTime: Date.now() });
	});

	await ctx.tool.hook("execute.after", async (event) => {
			// Plan save triggers reviewer delegation reminder
			if (event.tool === "plan_save" && event.status === "completed") {
				 await ctx.session.synthetic({ sessionID: event.sessionID, text: `<system-reminder>
Plan saved successfully. You MUST now delegate to the reviewer:
1. Use the \`delegate\` tool to send the plan to the \`reviewer\` agent
2. The reviewer will load \`plan-review\` and \`code-philosophy\` skills
3. Use \`plan_read\` to get the plan content for the delegation prompt
4. This is NON-BLOCKING - continue work while review runs in background
</system-reminder>` });
				return;
			}

			// Coder task completion tracking
			if (!activeCoderCalls.has(event.id)) return;

			activeCoderCalls.delete(event.id);

			if (activeCoderCalls.size === 0) {
				if (event.status !== "completed") return;
				await ctx.session.synthetic({ sessionID: event.sessionID, text: `<system-reminder>
Coder task complete. Proceed to code review:
1. Delegate to \`reviewer\` agent with the changed files
2. Include findings in your completion report
3. Offer to fix any critical/major issues found
</system-reminder>` });
			}
	});

	await ctx.session.hook("compaction", async (event) => {
			const rootID = await getRootSessionID(event.sessionID);
			const planPath = path.join(baseDir, rootID, "plan.md");

			let planContent: string | null = null;
			try {
				planContent = await fs.readFile(planPath, "utf8");
			} catch (error) {
				if (!isNodeError(error) || error.code !== "ENOENT") throw error;
			}

			if (!planContent) return;

			// Extract current task from plan
			const currentMatch = planContent.match(/← CURRENT/);
			let currentTask: string | null = null;
			if (currentMatch?.index !== undefined) {
				const start = Math.max(0, currentMatch.index - 100);
				const end = currentMatch.index + 50;
				currentTask =
					planContent.slice(start, end).match(/\d+\.\d+ [^\n←]+/)?.[0] ?? null;
			}

			event.system.push({ type: "text", text: `<workspace-context>
## Current Plan
${planContent}

## Resume Point
${currentTask ? `Current task: ${currentTask}` : "No task marked as CURRENT"}

## Verification
To verify any cited decision, use \`delegation_read("ref:id")\`.
</workspace-context>` });
	});
	},
});
