import { createHash, randomUUID } from "node:crypto";
import {
	lstat,
	mkdir,
	readFile,
	rename,
	unlink,
	writeFile,
} from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import {
	withFileMutationQueue,
	type ExtensionAPI,
} from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { parse } from "yaml";

const MAX_BYTES = 50 * 1024;

function maskFencedCode(content: string): string {
	let fence: { marker: string; length: number } | undefined;
	return content.replace(/[^\n]+/g, (line) => {
		if (fence) {
			const closing = line.match(/^ {0,3}(`+|~+)[ \t]*$/);
			if (closing && closing[1][0] === fence.marker && closing[1].length >= fence.length)
				fence = undefined;
			return " ".repeat(line.length);
		}
		const opening = line.match(/^ {0,3}(`{3,}|~{3,})(.*)$/);
		if (!opening || (opening[1][0] === "`" && opening[2].includes("`")))
			return line;
		fence = { marker: opening[1][0], length: opening[1].length };
		return " ".repeat(line.length);
	});
}

function validatePlan(content: string): string[] {
	if (!content.trim())
		throw new Error(
			"Plan is empty. Load the plan-protocol skill for the required format.",
		);
	if (Buffer.byteLength(content) > MAX_BYTES)
		throw new Error("Plan exceeds the 50 KiB limit.");
	const normalized = content.replace(/\r\n/g, "\n");
	const frontmatter = normalized.match(/^---\n([\s\S]*?)\n---(?:\n|$)/);
	if (!frontmatter)
		throw new Error(
			"Missing YAML frontmatter (status, phase, updated). Load plan-protocol.",
		);
	let metadata: unknown;
	try {
		metadata = parse(frontmatter[1] ?? "");
	} catch {
		throw new Error("Invalid YAML frontmatter. Load plan-protocol.");
	}
	if (!metadata || typeof metadata !== "object" || Array.isArray(metadata))
		throw new Error("Frontmatter must be a YAML mapping.");
	const fields = Object.fromEntries(Object.entries(metadata));
	if (
		!["not-started", "in-progress", "complete", "blocked"].includes(
			fields.status,
		)
	)
		throw new Error("Invalid frontmatter status.");
	if (!Number.isInteger(fields.phase) || fields.phase < 1)
		throw new Error("Frontmatter phase must be a positive integer.");
	if (
		typeof fields.updated !== "string" ||
		!/^\d{4}-\d{2}-\d{2}$/.test(fields.updated)
	)
		throw new Error("Frontmatter updated must be YYYY-MM-DD.");
	const structuralContent = maskFencedCode(normalized);
	const goalSection = structuralContent.match(
		/^##[ \t]+Goal[ \t]*\n([\s\S]*?)(?=^#{1,2}[ \t]|(?![\s\S]))/m,
	);
	const goal = goalSection?.index === undefined || goalSection[1] === undefined
		? undefined
		: normalized.slice(
				goalSection.index + goalSection[0].length - goalSection[1].length,
				goalSection.index + goalSection[0].length,
			).trim();
	if (!goal || goal.length < 10)
		throw new Error("A ## Goal section of at least 10 characters is required.");
	const sections = [
		...structuralContent.matchAll(
			/^##[ \t]+([^\n]*)\n?([\s\S]*?)(?=^#{1,2}[ \t]|(?![\s\S]))/gm,
		),
	];
	const phases = sections.filter((section) =>
		/^Phase\b/i.test(section[1] ?? ""),
	);
	if (!phases.length)
		throw new Error("At least one phase with a status marker is required.");
	const statuses: string[] = [];
	for (const section of phases) {
		const heading = section[1] ?? "";
		const phase = heading.match(
			/^Phase (\d+):[ \t]*(\S.*?)[ \t]+\[([^\]]+)\][ \t]*$/,
		);
		if (!phase) throw new Error(`Invalid phase heading: ${heading}`);
		const status = phase[3] ?? "";
		statuses.push(status);
		if (!["PENDING", "IN PROGRESS", "COMPLETE", "BLOCKED"].includes(status))
			throw new Error(`Invalid status for phase ${phase[1]}.`);
		if (!/^- \[[ x]\] (?:\*\*)?\d+\.\d+\s+\S/m.test(section[2] ?? ""))
			throw new Error(`Phase ${phase[1]} requires a numbered task (e.g. 1.1).`);
	}
	if ((structuralContent.match(/← CURRENT/g) ?? []).length > 1)
		throw new Error("Only one task may be marked ← CURRENT.");
	return statuses.filter((status) => status === "IN PROGRESS").length > 1
		? ["Multiple phases are IN PROGRESS; only one should be active."]
		: [];
}

function planPath(cwd: string, sessionId: string): string {
	if (!sessionId)
		throw new Error("Cannot resolve plan: session ID is missing.");
	const id = createHash("sha256").update(sessionId).digest("hex");
	return join(resolve(cwd), ".agents", "plans", `${id}.md`);
}

async function rootPlanIdentity(ctx: {
	cwd: string;
	sessionManager: {
		getSessionId(): string;
		getHeader(): { parentSession?: string } | null;
	};
}): Promise<{ cwd: string; id: string }> {
	let id = ctx.sessionManager.getSessionId();
	let cwd = ctx.cwd;
	let parent = ctx.sessionManager.getHeader()?.parentSession;
	const visited = new Set<string>();
	while (parent) {
		const file = resolve(parent);
		if (visited.has(file))
			throw new Error(`Cannot resolve plan: session ancestry cycle at ${file}`);
		visited.add(file);
		let header: unknown;
		try {
			header = JSON.parse(
				(await readFile(file, "utf8")).split("\n", 1)[0] ?? "",
			);
		} catch (error) {
			throw new Error(
				`Cannot resolve plan: cannot read parent session ${file}`,
				{ cause: error },
			);
		}
		if (
			!header ||
			typeof header !== "object" ||
			!("type" in header) ||
			header.type !== "session" ||
			!("id" in header) ||
			typeof header.id !== "string" ||
			!header.id ||
			!("cwd" in header) ||
			typeof header.cwd !== "string" ||
			!header.cwd ||
			("parentSession" in header &&
				header.parentSession !== undefined &&
				(typeof header.parentSession !== "string" || !header.parentSession))
		)
			throw new Error(`Cannot resolve plan: invalid session header in ${file}`);
		id = header.id;
		cwd = header.cwd;
		parent =
			"parentSession" in header && typeof header.parentSession === "string"
				? resolve(dirname(file), header.parentSession)
				: undefined;
	}
	return { cwd, id };
}

async function checkDirectory(path: string, create: boolean): Promise<void> {
	if (create) {
		try {
			await mkdir(path);
		} catch (error) {
			if (!(
				error instanceof Error &&
				"code" in error &&
				error.code === "EEXIST"
			))
				throw error;
		}
	}
	const info = await lstat(path);
	if (!info.isDirectory() || info.isSymbolicLink())
		throw new Error(
			`Plan directory must be a real directory, not a symlink: ${path}`,
		);
}

async function preparePath(
	cwd: string,
	sessionId: string,
	create: boolean,
): Promise<string> {
	const root = resolve(cwd);
	await checkDirectory(join(root, ".agents"), create);
	await checkDirectory(join(root, ".agents", "plans"), create);
	return planPath(root, sessionId);
}

function missing(error: unknown): boolean {
	return error instanceof Error && "code" in error && error.code === "ENOENT";
}

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "plan_save",
		label: "Save plan",
		description:
			"Save the root session's shared implementation plan under .agents/plans, replacing the entire plan. Load plan-protocol and provide complete markdown (maximum 50 KiB). Before updating, coordinate a single writer and use plan_read to get the latest version; do not write concurrently.",
		parameters: Type.Object(
			{ content: Type.String({ description: "Complete plan markdown" }) },
			{ additionalProperties: false },
		),
		async execute(_id, { content }, signal, _update, ctx) {
			const warnings = validatePlan(content);
			const root = await rootPlanIdentity(ctx);
			const target = await preparePath(root.cwd, root.id, true);
			return withFileMutationQueue(target, async () => {
				signal?.throwIfAborted();
				const temp = `${target}.${randomUUID()}.tmp`;
				try {
					await writeFile(temp, content, {
						encoding: "utf8",
						flag: "wx",
						mode: 0o600,
					});
					signal?.throwIfAborted();
					await rename(temp, target);
				} finally {
					await unlink(temp).catch((error: unknown) => {
						if (!missing(error)) throw error;
					});
				}
				return {
					content: [
						{
							type: "text",
							text: `Plan saved to ${target}.${warnings.length ? ` Warning: ${warnings.join(" ")}` : ""}`,
						},
					],
					details: { path: target, warnings },
				};
			});
		},
	});
	pi.registerTool({
		name: "plan_read",
		label: "Read plan",
		description:
			"Read the root session's shared implementation plan from .agents/plans (up to 50 KiB). Use after the planner completes, before implementation or review, when resuming planned work after compaction, and before updating with plan_save. Reports when no plan exists.",
		parameters: Type.Object(
			{ reason: Type.String({ description: "Why the plan is needed" }) },
			{ additionalProperties: false },
		),
		async execute(_id, _params, _signal, _update, ctx) {
			const root = await rootPlanIdentity(ctx);
			let target: string;
			try {
				target = await preparePath(root.cwd, root.id, false);
				const info = await lstat(target);
				if (!info.isFile() || info.isSymbolicLink())
					throw new Error(
						`Plan must be a regular file, not a symlink: ${target}`,
					);
				if (info.size > MAX_BYTES)
					throw new Error(
						`Plan exceeds 50 KiB; read it directly at ${target}.`,
					);
				const content = await readFile(target, "utf8");
				if (Buffer.byteLength(content) > MAX_BYTES)
					throw new Error(
						`Plan exceeds 50 KiB; read it directly at ${target}.`,
					);
				return {
					content: [{ type: "text", text: content }],
					details: { path: target },
				};
			} catch (error) {
				if (missing(error))
					return {
						content: [
							{
								type: "text",
								text: "No plan found for this session. Use plan_save to create one.",
							},
						],
						details: {},
					};
				throw error;
			}
		},
	});
}
