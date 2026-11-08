import { mkdtemp, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { SessionManager } from "@earendil-works/pi-coding-agent";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import planExtension from "./index";
import type {
	ExtensionAPI,
	ExtensionContext,
	ToolDefinition,
} from "@earendil-works/pi-coding-agent";

const plan = `---
status: in-progress
phase: 1
updated: 2026-01-02
---
# Implementation Plan
## Goal
Implement a useful change.
## Phase 1: Work [IN PROGRESS]
- [ ] **1.1 Do work** ← CURRENT
`;
const dirs: string[] = [];
afterEach(async () => {
	await Promise.all(dirs.splice(0).map((dir) => rm(dir, { recursive: true, force: true })));
});

function setup(cwd: string, session: string, manager?: SessionManager) {
	const tools = new Map<string, ToolDefinition>();
	planExtension({
		registerTool: (tool: ToolDefinition) => tools.set(tool.name, tool),
	} as unknown as ExtensionAPI);
	const ctx = { cwd, sessionManager: manager ?? { getSessionId: () => session, getHeader: () => null } } as ExtensionContext;
	return {
		tools,
		save: (content: string) =>
			tools.get("plan_save")!.execute("id", { content }, undefined, undefined, ctx),
		read: () =>
			tools.get("plan_read")!.execute("id", { reason: "test" }, undefined, undefined, ctx),
	};
}
async function project() {
	const dir = await mkdtemp(join(tmpdir(), "pi-plan-"));
	dirs.push(dir);
	return dir;
}

describe("session plans", () => {
	it("describes shared plan handoff and safe complete updates", async () => {
		const { tools } = setup(await project(), "a");
		const save = tools.get("plan_save")?.description;
		const read = tools.get("plan_read")?.description;
		expect(save).toContain("root session's shared implementation plan");
		expect(save).toContain("replacing the entire plan");
		expect(save).toContain("coordinate a single writer and use plan_read");
		expect(read).toContain("after the planner completes");
		expect(read).toContain("resuming planned work after compaction");
		expect(read).toContain("Reports when no plan exists");
	});
	it("saves, resumes and isolates sessions even with unsafe IDs", async () => {
		const dir = await project();
		const a = setup(dir, "../../a");
		const b = setup(dir, "b");
		expect((await b.read()).content[0]).toMatchObject({
			text: expect.stringContaining("No plan found"),
		});
		const saved = await a.save(plan);
		const details = saved.details as { path: string };
		expect(details.path.startsWith(join(dir, ".agents", "plans"))).toBe(true);
		expect(await readFile(details.path, "utf8")).toBe(plan);
		expect((await setup(dir, "../../a").read()).content[0]).toMatchObject({ text: plan });
		await b.save(plan.replace("useful", "different"));
		expect((await a.read()).content[0]).toMatchObject({ text: plan });
	});
	it("rejects invalid plans without replacing a saved plan", async () => {
		const dir = await project();
		const a = setup(dir, "a");
		await a.save(plan);
		await expect(a.save(plan + "- [ ] 1.2 More ← CURRENT\n")).rejects.toThrow("Only one task");
		await expect(a.save("bad")).rejects.toThrow("frontmatter");
		expect((await a.read()).content[0]).toMatchObject({ text: plan });
	});
	it("shares plans across real Pi session ancestry and isolates other roots", async () => {
		const dir = await project();
		const sessions = await project();
		const root = SessionManager.create(dir, sessions);
		const rootFile = root.getSessionFile();
		if (!rootFile) throw new Error("Expected persistent root session");
		await writeFile(rootFile, `${JSON.stringify(root.getHeader())}\n`);
		const child = SessionManager.create(dir, sessions);
		const childFile = child.newSession({ parentSession: rootFile });
		if (!childFile) throw new Error("Expected persistent child session");
		await writeFile(childFile, `${JSON.stringify(child.getHeader())}\n`);
		const grandchild = SessionManager.create(dir, sessions);
		grandchild.newSession({ parentSession: childFile });
		const parentPlan = setup(dir, "", root);
		const childPlan = setup(dir, "", child);
		const nestedPlan = setup(dir, "", grandchild);
		await childPlan.save(plan);
		expect((await parentPlan.read()).content[0]).toMatchObject({ text: plan });
		await parentPlan.save(plan.replace("useful", "updated"));
		expect((await nestedPlan.read()).content[0]).toMatchObject({ text: plan.replace("useful", "updated") });
		expect((await setup(dir, "other").read()).content[0]).toMatchObject({ text: expect.stringContaining("No plan found") });
	});
	it("fails loudly when ancestry is missing", async () => {
		const dir = await project();
		const child = SessionManager.create(dir, await project());
		child.newSession({ parentSession: join(dir, "missing.jsonl") });
		await expect(setup(dir, "", child).read()).rejects.toThrow("cannot read parent session");
	});
	it.each(["Phase 2: Missing status", "Phase two: Bad [PENDING]", "Phase 2: Work [UNKNOWN]", "Phase 2: Work [PENDING]"])("rejects malformed additional heading or tasks: %s", async (heading) => {
		const a = setup(await project(), "a");
		await a.save(plan);
		await expect(a.save(`${plan}\n## ${heading}\nNo numbered task\n`)).rejects.toThrow();
		expect((await a.read()).content[0]).toMatchObject({ text: plan });
	});
	it("refuses symlinked plan directories", async () => {
		const dir = await project();
		const outside = await project();
		await symlink(outside, join(dir, ".agents"));
		await expect(setup(dir, "a").save(plan)).rejects.toThrow("real directory");
	});
});
