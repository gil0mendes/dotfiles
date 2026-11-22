import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
	DefaultPackageManager,
	SettingsManager,
	type ExtensionAPI,
} from "@earendil-works/pi-coding-agent";
import { TogglePicker, type ToggleOption } from "./picker.js";
import extension, { toggleFilter } from "./index.js";

const state = vi.hoisted(() => ({ agentDir: "" }));
vi.mock("@earendil-works/pi-coding-agent", async (importOriginal) => ({
	...(await importOriginal<typeof import("@earendil-works/pi-coding-agent")>()),
	getAgentDir: () => state.agentDir,
}));

let root = "";
afterEach(() => {
	if (root) rmSync(root, { recursive: true, force: true });
	vi.restoreAllMocks();
});

function fixture(settings: object = {}) {
	root = mkdtempSync(join(tmpdir(), "extension-toggle-"));
	state.agentDir = join(root, "agent");
	const extensions = join(state.agentDir, "extensions");
	mkdirSync(extensions, { recursive: true });
	const target = join(extensions, "example.ts");
	writeFileSync(target, "export default function() {}\n");
	writeFileSync(join(state.agentDir, "settings.json"), JSON.stringify(settings));
	return target;
}

function handler() {
	const registerCommand = vi.fn<ExtensionAPI["registerCommand"]>();
	extension({ registerCommand, registerShortcut: vi.fn() });
	return registerCommand.mock.calls[0][1].handler;
}

function context(input = [" ", "\r"], confirm = false, trusted = false) {
	const frames: string[] = [];
	const ui = {
		custom: vi.fn(async (factory: (...args: never[]) => TogglePicker<ToggleOption>) => {
			let result: unknown;
			const picker = Reflect.apply(factory, undefined, [
				{ terminal: { rows: 24 }, requestRender() {} },
				{ fg: (_color: string, text: string) => text },
				undefined,
				(value: unknown) => {
					result = value;
				},
			]);
			frames.push(...picker.render(200));
			for (const key of input) picker.handleInput(key);
			return result;
		}),
		confirm: vi.fn(async () => confirm),
		notify: vi.fn(),
	};
	const ctx = {
		cwd: root,
		mode: "tui",
		ui,
		isProjectTrusted: () => trusted,
		waitForIdle: async () => {},
		reload: vi.fn(async () => {}),
	};
	return { ...ctx, frames, run: () => Reflect.apply(handler(), undefined, ["", ctx]) };
}

describe("toggleFilter", () => {
	it("replaces exact overrides without disturbing other filters", () => {
		expect(toggleFilter(["!**/test*", "-/a", "+/ab"], "/a", true)).toEqual([
			"!**/test*",
			"+/ab",
			"+/a",
		]);
		expect(toggleFilter(["+/a"], "/a", false)).toEqual(["-/a"]);
	});

	it("replaces relative overrides without removing configured paths", () => {
		expect(
			toggleFilter(["extensions", "-./extensions/a.ts"], "/repo/.pi/extensions/a.ts", true, [
				"/repo/.pi",
			]),
		).toEqual(["extensions", "+/repo/.pi/extensions/a.ts"]);
	});

	it("replaces overrides targeting a skill directory or its SKILL.md", () => {
		expect(
			toggleFilter(
				["-skills/a", "-skills/a/SKILL.md", "-skills/ab"],
				"/agent/skills/a/SKILL.md",
				true,
				["/agent"],
				true,
			),
		).toEqual(["-skills/ab", "+/agent/skills/a/SKILL.md"]);
	});
});

describe("extension-toggle command", () => {
	it("discovers disabled extensions and enables them without reloading when declined", async () => {
		const target = fixture();
		writeFileSync(
			join(state.agentDir, "settings.json"),
			JSON.stringify({ extensions: [`-${target}`] }),
		);
		const ctx = context();
		await ctx.run();
		expect(ctx.frames.join("\n")).toContain("[ ] Global · extensions · extensions/example.ts");
		expect(
			JSON.parse(readFileSync(join(state.agentDir, "settings.json"), "utf8")).extensions,
		).toContain(`+${target}`);
		expect(ctx.reload).not.toHaveBeenCalled();
		expect(ctx.ui.notify).toHaveBeenCalledWith(
			"Saved 1 resource change. Run /reload to apply.",
			"info",
		);
	});

	it("protects itself and reloads after a successful save when confirmed", async () => {
		const target = fixture();
		const self = dirname(fileURLToPath(import.meta.url));
		writeFileSync(join(state.agentDir, "settings.json"), JSON.stringify({ extensions: [self] }));
		const ctx = context(undefined, true);
		await ctx.run();
		expect(ctx.frames.filter((line) => line.includes("Global · extensions"))).toHaveLength(1);
		expect(
			JSON.parse(readFileSync(join(state.agentDir, "settings.json"), "utf8")).extensions,
		).toContain(`-${target}`);
		expect(ctx.reload).toHaveBeenCalledOnce();
	});

	it("reports persistence errors without offering reload", async () => {
		fixture();
		vi.spyOn(SettingsManager.prototype, "flush").mockRejectedValueOnce(new Error("disk full"));
		const ctx = context(undefined, true);
		await ctx.run();
		expect(ctx.ui.notify).toHaveBeenCalledWith("Extension toggle failed: disk full", "error");
		expect(ctx.ui.confirm).not.toHaveBeenCalled();
		expect(ctx.reload).not.toHaveBeenCalled();
	});

	it("writes trusted project changes to project settings only", async () => {
		fixture();
		const projectDir = join(root, ".pi");
		mkdirSync(join(projectDir, "prompts"), { recursive: true });
		const prompt = join(projectDir, "prompts", "example.md");
		writeFileSync(prompt, "Example prompt\n");
		const ctx = context(["p", "r", "o", "m", "p", "t", " ", "\r"], false, true);
		await ctx.run();
		expect(JSON.parse(readFileSync(join(projectDir, "settings.json"), "utf8")).prompts).toEqual([
			`-${prompt}`,
		]);
		expect(JSON.parse(readFileSync(join(state.agentDir, "settings.json"), "utf8"))).toEqual({});
	});

	it("resolver retains disabled discovered entries", async () => {
		const target = fixture();
		writeFileSync(
			join(state.agentDir, "settings.json"),
			JSON.stringify({ extensions: [`-${target}`] }),
		);
		const settingsManager = SettingsManager.create(root, state.agentDir);
		const manager = new DefaultPackageManager({
			cwd: root,
			agentDir: state.agentDir,
			settingsManager,
		});
		const resources = await manager.resolve(async () => "skip");
		expect(resources.extensions).toEqual(
			expect.arrayContaining([expect.objectContaining({ path: target, enabled: false })]),
		);
	});
});
