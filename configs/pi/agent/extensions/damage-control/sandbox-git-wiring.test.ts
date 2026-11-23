// Run from configs/pi: bunx --no-install vitest run agent/extensions/damage-control/sandbox-git-wiring.test.ts
// Verifies the sandbox is configured with the git metadata of a linked worktree.
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import type { SandboxRuntimeConfig } from "@carderne/sandbox-runtime";
import { afterAll, beforeAll, beforeEach, describe, expect, it, vi } from "vitest";

const mocks = vi.hoisted(() => ({
	initialize: vi.fn(async (_config: SandboxRuntimeConfig) => {}),
	reset: vi.fn(async () => {}),
}));

vi.mock("@carderne/sandbox-runtime", () => ({
	SandboxManager: { initialize: mocks.initialize, reset: mocks.reset },
}));
vi.mock("@earendil-works/pi-coding-agent", async (importOriginal) => ({
	...(await importOriginal<typeof import("@earendil-works/pi-coding-agent")>()),
	SettingsManager: { create: () => ({ getShellPath: () => undefined }) },
	getShellConfig: () => ({ shell: "/mock/shell" }),
	createLocalBashOperations: () => ({ exec: async () => ({ exitCode: 0 }) }),
	createBashToolDefinition: () => ({ name: "bash", execute: async () => ({ content: [] }) }),
}));
vi.mock("./config", () => {
	const current = {
		filesystem: { denyRead: [], allowWrite: ["."], denyWrite: [".env"] },
		bashToolPatterns: [],
		strictModeAllowedList: [],
	};
	return {
		DEFAULT_CONFIG_PATH: "/fixture/rules.yaml",
		configs: {
			source: "/fixture/rules.yaml",
			current,
			load: () => ({ error: undefined }),
			totalRules: () => 0,
		},
	};
});

type Handler = (event: unknown, ctx: ExtensionContext) => Promise<void>;

function git(cwd: string, ...args: string[]): void {
	execFileSync(
		"git",
		[
			"-c",
			"user.name=test",
			"-c",
			"user.email=test@example.com",
			"-c",
			"commit.gpgsign=false",
			...args,
		],
		{ cwd, stdio: "ignore" },
	);
}

function uiContext(cwd: string): ExtensionContext {
	return {
		cwd,
		ui: { setStatus: vi.fn(), notify: vi.fn() },
	} as unknown as ExtensionContext;
}

async function loadExtension() {
	vi.resetModules();
	const { default: damageControl } = await import("./index");
	const handlers = new Map<string, Handler>();
	const commands = new Map<
		string,
		{ handler: (args: string, ctx: ExtensionContext) => Promise<void> }
	>();
	damageControl({
		on: (event: string, handler: Handler) => handlers.set(event, handler),
		registerTool: () => {},
		registerCommand: (
			name: string,
			command: { handler: (args: string, ctx: ExtensionContext) => Promise<void> },
		) => commands.set(name, command),
		events: { emit: vi.fn() },
	} as unknown as ExtensionAPI);
	return { handlers, commands };
}

function lastSandboxFilesystem() {
	const config = mocks.initialize.mock.calls.at(-1)?.[0];
	if (!config?.filesystem) throw new Error("sandbox was not initialized");
	return config.filesystem;
}

describe("sandbox git metadata wiring", () => {
	let root: string;
	let mainCheckout: string;
	let worktree: string;
	let notARepo: string;

	beforeAll(() => {
		root = fs.realpathSync.native(fs.mkdtempSync(path.join(os.tmpdir(), "dc-git-wiring-")));
		mainCheckout = path.join(root, "main");
		worktree = path.join(root, "worktrees", "feature");
		notARepo = path.join(root, "not-a-repo");
		for (const dir of [mainCheckout, notARepo]) fs.mkdirSync(dir, { recursive: true });
		git(mainCheckout, "init", "-q", "-b", "main");
		git(mainCheckout, "commit", "-q", "--allow-empty", "-m", "init");
		git(mainCheckout, "worktree", "add", "-q", "-b", "feature", worktree);
	});

	afterAll(() => {
		fs.rmSync(root, { recursive: true, force: true });
	});

	beforeEach(() => {
		mocks.initialize.mockClear();
		mocks.reset.mockClear();
	});

	it("allows the worktree's git dirs and protects hooks when the session starts", async () => {
		const { handlers } = await loadExtension();
		await handlers.get("session_start")?.({}, uiContext(worktree));

		const filesystem = lastSandboxFilesystem();
		const commonDir = path.join(mainCheckout, ".git");
		const gitDir = path.join(commonDir, "worktrees", "feature");
		expect(filesystem.allowWrite).toEqual(expect.arrayContaining([".", commonDir, gitDir]));
		expect(filesystem.denyWrite).toEqual(
			expect.arrayContaining([".env", path.join(commonDir, "hooks"), path.join(gitDir, "hooks")]),
		);
	});

	it("adds nothing outside a git repository", async () => {
		const { handlers } = await loadExtension();
		await handlers.get("session_start")?.({}, uiContext(notARepo));

		const filesystem = lastSandboxFilesystem();
		expect(filesystem.allowWrite).toEqual(["."]);
		expect(filesystem.denyWrite).toEqual([".env"]);
	});

	it("re-resolves the git metadata when rules are reloaded", async () => {
		const { handlers, commands } = await loadExtension();
		await handlers.get("session_start")?.({}, uiContext(notARepo));
		expect(lastSandboxFilesystem().allowWrite).toEqual(["."]);

		await commands.get("damage-control-reload")?.handler("", uiContext(worktree));

		expect(lastSandboxFilesystem().allowWrite).toContain(path.join(mainCheckout, ".git"));
	});
});
