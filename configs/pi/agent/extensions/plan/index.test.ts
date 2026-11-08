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
	it.each(["LF", "CRLF"])("validates multiline sections and subsections with %s endings", async (lineEnding) => {
		const a = setup(await project(), "a");
		const content = `---
status: in-progress
phase: 1
updated: 2026-01-02
---
# Implementation Plan
## Goal
A short

but sufficiently detailed goal.
### Goal detail
More context.
## Phase 1: Work [IN PROGRESS]
Introduction to the phase.

### Tasks
- [ ] 1.1 Do work
## Phase 2: Finish [PENDING]
More introduction.

- [ ] 2.1 Finish work`;
		const input = lineEnding === "CRLF" ? content.replace(/\n/g, "\r\n") : content;
		await a.save(input);
		expect((await a.read()).content[0]).toMatchObject({ text: input });
	});
	it.each(["\t", " \t "])("accepts a Goal heading separated by %j", async (separator) => {
		const a = setup(await project(), "a");
		await expect(a.save(plan.replace("## Goal", `##${separator}Goal`))).resolves.toBeDefined();
	});
	it("accepts a multiline goal at EOF", async () => {
		const a = setup(await project(), "a");
		const content = plan.replace("## Goal\nImplement a useful change.\n", "") + "\n## Goal\nFirst line.\n\nSecond line.";
		await expect(a.save(content)).resolves.toBeDefined();
	});
	it.each([
		["empty goal", plan.replace("Implement a useful change.", "")],
		["short goal", plan.replace("Implement a useful change.", "Short")],
		["goal followed by a level-one heading", plan.replace("Implement a useful change.\n## Phase", "Short\n# Long unrelated heading\n## Phase")],
		["goal followed by a level-two heading", plan.replace("Implement a useful change.\n## Phase", "Short\n## Long unrelated heading\n## Phase")],
		["goal followed by a tab-separated level-one heading", plan.replace("Implement a useful change.\n## Phase", "Short\n#\tLong unrelated heading\n## Phase")],
		["goal followed by a tab-separated level-two heading", plan.replace("Implement a useful change.\n## Phase", "Short\n##\tLong unrelated heading\n## Phase")],
	])("rejects %s", async (_description, content) => {
		const a = setup(await project(), "a");
		await expect(a.save(content)).rejects.toThrow("A ## Goal section of at least 10 characters is required.");
	});
	it.each(["# Other section", "## Other section", "## Phase 2: Next [PENDING]"])("does not take a task from the following heading: %s", async (heading) => {
		const a = setup(await project(), "a");
		const content = plan.replace("- [ ] **1.1 Do work** ← CURRENT", `Introduction only.\n\n${heading}\n- [ ] 2.1 A task in another section`);
		await expect(a.save(content)).rejects.toThrow("Phase 1 requires a numbered task");
	});
	it.each(["```", "~~~"])("ignores headings inside %s fences in goals and phases", async (fence) => {
		const a = setup(await project(), "a");
		const content = plan
			.replace("Implement a useful change.", `A sufficiently long goal.\n${fence}markdown\n# False boundary\n## Phase 9: Fake [UNKNOWN]\n${fence}\nMore goal context.`)
			.replace("- [ ] **1.1 Do work** ← CURRENT", `${fence}\n# False boundary\n## Phase 9: Fake [UNKNOWN]\n${fence}\n- [ ] **1.1 Do work** ← CURRENT`);
		await expect(a.save(content)).resolves.toBeDefined();
	});
	it.each(["```", "~~~"])("does not count tasks inside %s fences", async (fence) => {
		const a = setup(await project(), "a");
		const content = plan.replace("- [ ] **1.1 Do work** ← CURRENT", `${fence}\n- [ ] 1.1 Fake task\n${fence}`);
		await expect(a.save(content)).rejects.toThrow("Phase 1 requires a numbered task");
	});
	it.each(["```", "~~~"])("counts CURRENT markers only outside %s fences", async (fence) => {
		const a = setup(await project(), "a");
		const example = `\n${fence}markdown\n- [ ] 1.2 Example ← CURRENT\n- [ ] 1.3 Example ← CURRENT\n${fence}\n`;
		await expect(a.save(plan + example)).resolves.toBeDefined();
		await expect(a.save(plan + example + "- [ ] 1.4 Real task ← CURRENT\n")).rejects.toThrow("Only one task");
	});
	it("refuses symlinked plan directories", async () => {
		const dir = await project();
		const outside = await project();
		await symlink(outside, join(dir, ".agents"));
		await expect(setup(dir, "a").save(plan)).rejects.toThrow("real directory");
	});
});
