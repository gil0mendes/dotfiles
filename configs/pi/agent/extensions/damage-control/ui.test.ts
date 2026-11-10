// Run from configs/pi: bunx --no-install vitest run agent/extensions/damage-control/ui.test.ts
// Isolated mocks only: TUI approval fails closed; non-TUI sandbox results are returned unchanged.
import { homedir } from "node:os";
import type {
	AgentToolResult,
	BashToolInput,
	ExtensionAPI,
	ExtensionContext,
	ExtensionHandler,
	ExtensionToolContext,
	ToolCallEvent,
	ToolCallEventResult,
} from "@earendil-works/pi-coding-agent";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { createPathAccessPromptComponent, type PromptResult } from "./prompt";

const mocks = vi.hoisted(() => ({
	execute: vi.fn(
		async (): Promise<AgentToolResult<undefined>> => ({
			content: [{ type: "text", text: "sandbox denied" }],
			details: undefined,
		}),
	),
	violations: vi.fn(() => [
		{
			timestamp: new Date(Date.now() + 1000),
			line: "bash(1) deny file-read-data /outside/dir/file.txt",
		},
	]),
	initialize: vi.fn(async () => {}),
	reset: vi.fn(async () => {}),
	mkdir: vi.fn(),
	write: vi.fn(),
}));

vi.mock("node:fs", async (importOriginal) => ({
	...(await importOriginal<typeof import("node:fs")>()),
	mkdirSync: mocks.mkdir,
	writeFileSync: mocks.write,
	existsSync: () => false,
}));
vi.mock("@carderne/sandbox-runtime", () => ({
	SandboxManager: {
		initialize: mocks.initialize,
		reset: mocks.reset,
		getSandboxViolationStore: () => ({ getViolationsForCommand: mocks.violations }),
	},
}));
vi.mock("@earendil-works/pi-coding-agent", async (importOriginal) => ({
	...(await importOriginal<typeof import("@earendil-works/pi-coding-agent")>()),
	SettingsManager: { create: () => ({ getShellPath: () => undefined }) },
	getShellConfig: () => ({ shell: "/mock/shell" }),
	createLocalBashOperations: () => ({ exec: vi.fn() }),
	createBashToolDefinition: () => ({ name: "bash", execute: mocks.execute }),
}));
vi.mock("./config", () => ({
	DEFAULT_CONFIG_PATH: "/fixture/rules.yaml",
	configs: {
		source: "/fixture/rules.yaml",
		current: {
			filesystem: {
				denyRead: ["/"],
				allowRead: ["/workspace"],
				allowWrite: ["/workspace"],
				denyWrite: ["/outside/denied"],
			},
			bashToolPatterns: [],
			strictModeAllowedList: [],
		},
	},
}));
vi.mock("./filesystem", async () => {
	const path = await import("node:path");
	const os = await import("node:os");
	type Filesystem = import("./types").FilesystemGateConfig;
	const matches = (target: string, patterns: readonly string[] = []) =>
		patterns.some(
			(pattern) =>
				target === pattern.replace(/\/$/, "") ||
				target.startsWith(`${pattern.replace(/\/$/, "")}/`),
		);
	return {
		canonicalizePath: (target: string, cwd: string) => path.resolve(cwd, target),
		normalizeForDisplay: (target: string) => target,
		storageForm: (target: string, _cwd: string, directory: boolean) =>
			`${target}${directory ? "/" : ""}`,
		isGrantTooBroad: (target: string) => target === os.homedir(),
		isWriteDenied: (target: string, fs: Filesystem) => matches(target, fs.denyWrite),
		shouldPromptForRead: (target: string, fs: Filesystem) => !matches(target, fs.allowRead),
		shouldPromptForWrite: (target: string, fs: Filesystem) => !matches(target, fs.allowWrite),
		withSessionAllowances: (fs: Filesystem, read: string[], write: string[]) => ({
			...fs,
			allowRead: [...(fs.allowRead ?? []), ...read],
			allowWrite: [...fs.allowWrite, ...write],
		}),
	};
});

type ToolHandler = ExtensionHandler<ToolCallEvent, ToolCallEventResult>;
type BashExecute = (
	id: string,
	params: BashToolInput,
	signal: AbortSignal | undefined,
	onUpdate: undefined,
	ctx: ExtensionToolContext,
) => Promise<AgentToolResult<unknown>>;

async function setup() {
	vi.resetModules();
	const { default: damageControl } = await import("./index");
	let handler: ToolHandler | undefined;
	let execute: BashExecute | undefined;
	const registration = {
		on: (event: string, callback: unknown) => {
			if (event === "tool_call") handler = callback as ToolHandler;
			return () => {};
		},
		registerTool: (tool: { name: string; execute: BashExecute }) => {
			if (tool.name === "bash") execute = tool.execute;
		},
		registerCommand: () => {},
		events: { emit: vi.fn() },
	};
	damageControl(registration as unknown as ExtensionAPI);
	if (!handler || !execute) throw new Error("Damage Control registration incomplete");
	return { handler, execute };
}

function context(mode: ExtensionContext["mode"] = "tui", signal?: AbortSignal) {
	const custom = vi.fn(
		async (
			_factory: Parameters<ExtensionContext["ui"]["custom"]>[0],
			_options?: Parameters<ExtensionContext["ui"]["custom"]>[1],
		): Promise<PromptResult | undefined> => "deny",
	);
	const ui = { custom, confirm: vi.fn(), select: vi.fn(), notify: vi.fn(), setStatus: vi.fn() };
	const abort = vi.fn();
	const ctx = {
		mode,
		hasUI: mode === "tui" || mode === "rpc",
		cwd: "/workspace",
		signal,
		ui,
		abort,
	} as unknown as ExtensionToolContext;
	return { ctx, ui, abort };
}

function event(toolName: "read" | "write" | "edit", path = "/outside/dir/file.txt"): ToolCallEvent {
	return { type: "tool_call", toolName, toolCallId: "test", input: { path } };
}

beforeEach(() => {
	vi.clearAllMocks();
});

describe("filesystem approval", () => {
	it.each(["read", "write", "edit"] as const)(
		"marks %s approval as permission with normalized path",
		async (tool) => {
			const { handler } = await setup();
			const { ctx, ui } = context();
			expect(await handler(event(tool, "/outside/dir/../dir/file.txt"), ctx)).toMatchObject({
				block: true,
			});
			expect(ui.custom).toHaveBeenCalledOnce();
			expect(ui.custom.mock.calls[0]?.[1]).toEqual({
				blocked: {
					kind: "permission",
					message: `Allow ${tool} ${tool === "read" ? "read" : "write"} access to /outside/dir/file.txt?`,
				},
			});
			expect(mocks.write).not.toHaveBeenCalled();
		},
	);

	it.each(["deny", undefined] as const)("blocks %s without changing allowances", async (choice) => {
		const { handler } = await setup();
		const { ctx, ui, abort } = context();
		ui.custom.mockResolvedValue(choice);
		expect(await handler(event("read"), ctx)).toMatchObject({ block: true });
		expect(await handler(event("read"), ctx)).toMatchObject({ block: true });
		expect(ui.custom).toHaveBeenCalledTimes(2);
		expect(abort).toHaveBeenCalled();
		expect(mocks.write).not.toHaveBeenCalled();
		expect(mocks.reset).not.toHaveBeenCalled();
	});

	it.each(["rpc", "print", "json"] as const)("denies %s without dialogs", async (mode) => {
		const { handler } = await setup();
		const { ctx, ui } = context(mode);
		for (const tool of ["read", "write", "edit"] as const)
			expect(await handler(event(tool), ctx)).toMatchObject({ block: true });
		expect(ui.custom).not.toHaveBeenCalled();
		expect(ui.confirm).not.toHaveBeenCalled();
		expect(ui.select).not.toHaveBeenCalled();
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it("blocks pre-abort and late approval after abort", async () => {
		const { handler } = await setup();
		const controller = new AbortController();
		const { ctx, ui } = context("tui", controller.signal);
		let resolve: (value: PromptResult) => void = () => {
			throw new Error("missing resolver");
		};
		ui.custom.mockImplementation(
			() =>
				new Promise((done) => {
					resolve = done;
				}),
		);
		const pending = handler(event("write"), ctx);
		controller.abort();
		resolve("allow-file-always");
		expect(await pending).toMatchObject({ block: true });
		expect(await handler(event("write"), ctx)).toMatchObject({ block: true });
		expect(ui.custom).toHaveBeenCalledOnce();
		const fresh = context();
		expect(await handler(event("write"), fresh.ctx)).toMatchObject({ block: true });
		expect(fresh.ui.custom).toHaveBeenCalledOnce();
		expect(mocks.write).not.toHaveBeenCalled();
		expect(mocks.reset).not.toHaveBeenCalled();
	});

	it("propagates rejected prompts without granting", async () => {
		const { handler } = await setup();
		const { ctx, ui } = context();
		ui.custom.mockRejectedValueOnce(new Error("UI failed"));
		await expect(handler(event("read"), ctx)).rejects.toThrow("UI failed");
		expect(await handler(event("read"), ctx)).toMatchObject({ block: true });
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it.each([
		["allow-file-once", false, false, false],
		["allow-dir-once", false, false, false],
		["allow-file-session", true, false, false],
		["allow-dir-session", true, true, false],
		["allow-file-always", true, false, true],
		["allow-dir-always", true, true, true],
	] as const)("preserves %s scope", async (choice, session, directory, persisted) => {
		const { handler } = await setup();
		const { ctx, ui } = context();
		ui.custom.mockResolvedValueOnce(choice).mockResolvedValue("deny");
		expect(await handler(event("read"), ctx)).toMatchObject({ block: false });
		expect(await handler(event("read"), ctx)).toMatchObject({ block: !session });
		expect(await handler(event("read", "/outside/dir/other.txt"), ctx)).toMatchObject({
			block: !directory,
		});
		expect(mocks.write).toHaveBeenCalledTimes(persisted ? 1 : 0);
		if (persisted)
			expect(mocks.write.mock.calls[0]?.[1]).toContain(
				directory ? "/outside/dir/" : "/outside/dir/file.txt",
			);
	});

	it("keeps write session grants separate from read grants", async () => {
		const { handler } = await setup();
		const { ctx, ui } = context();
		ui.custom.mockResolvedValueOnce("allow-dir-session").mockResolvedValue("deny");
		expect(await handler(event("edit"), ctx)).toMatchObject({ block: false });
		expect(await handler(event("write", "/outside/dir/other.txt"), ctx)).toMatchObject({
			block: false,
		});
		expect(await handler(event("read"), ctx)).toMatchObject({ block: true });
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it("treats an overly broad directory as once without persistence", async () => {
		const { handler } = await setup();
		const { ctx, ui } = context();
		ui.custom.mockResolvedValueOnce("allow-dir-always").mockResolvedValue("deny");
		const target = `${homedir()}/file.txt`;
		expect(await handler(event("write", target), ctx)).toMatchObject({ block: false });
		expect(ui.notify).toHaveBeenCalledWith(expect.stringContaining("too broad"), "warning");
		expect(await handler(event("write", target), ctx)).toMatchObject({ block: true });
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it("respects denyWrite before approval", async () => {
		const { handler } = await setup();
		const { ctx, ui } = context();
		expect(await handler(event("write", "/outside/denied"), ctx)).toMatchObject({ block: true });
		expect(ui.custom).not.toHaveBeenCalled();
	});

	it.each([
		[true, 0, "allow-file-once"],
		[true, 1, "allow-file-session"],
		[true, 2, "allow-file-always"],
		[true, 3, "allow-dir-session"],
		[true, 4, "allow-dir-always"],
		[true, 5, "deny"],
		[false, 0, "allow-dir-once"],
		[false, 1, "allow-dir-session"],
		[false, 2, "allow-dir-always"],
		[false, 3, "deny"],
	] as const)("keeps prompt choice %s/%s as %s", (files, index, choice) => {
		const done = vi.fn();
		const component = createPathAccessPromptComponent(
			"read read",
			"/outside/file",
			"/outside",
			"/workspace",
			files,
		)(
			{ requestRender: vi.fn() },
			{ fg: (_color, text) => text, bg: (_color, text) => text, bold: (text) => text },
			undefined,
			done,
		);
		for (let i = 0; i < index; i++) component.handleInput("j");
		component.handleInput("\r");
		expect(done).toHaveBeenCalledWith(choice);
	});

	it("Escape on the existing prompt means deny", () => {
		const done = vi.fn();
		const component = createPathAccessPromptComponent(
			"read read",
			"/outside/file",
			"/outside",
			"/workspace",
			true,
		)(
			{ requestRender: vi.fn() },
			{ fg: (_color, text) => text, bg: (_color, text) => text, bold: (text) => text },
			undefined,
			done,
		);
		expect(component.render(100).join("\n")).toContain("Allow file this session");
		component.handleInput("\x1b");
		expect(done).toHaveBeenCalledWith("deny");
	});
});

describe("sandbox retry", () => {
	it.each(["rpc", "print", "json"] as const)(
		"returns original result in %s without inspecting violations",
		async (mode) => {
			const { execute } = await setup();
			const { ctx, ui } = context(mode);
			const result = await execute("id", { command: "fixture" }, undefined, undefined, ctx);
			expect(result).toBe(await mocks.execute.mock.results[0]?.value);
			expect(mocks.execute).toHaveBeenCalledOnce();
			expect(mocks.violations).not.toHaveBeenCalled();
			expect(ui.custom).not.toHaveBeenCalled();
		},
	);

	it.each(["deny", undefined] as const)("does not retry on %s", async (choice) => {
		const { execute } = await setup();
		const { ctx, ui } = context();
		ui.custom.mockResolvedValue(choice);
		await execute("id", { command: "fixture" }, undefined, undefined, ctx);
		expect(mocks.execute).toHaveBeenCalledOnce();
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it("preserves sandbox allow-once session allowance and retries once", async () => {
		const { execute, handler } = await setup();
		const { ctx, ui } = context();
		ui.custom.mockResolvedValue("allow-file-once");
		await execute("id", { command: "fixture" }, undefined, undefined, ctx);
		expect(mocks.execute).toHaveBeenCalledTimes(2);
		expect(await handler(event("read"), ctx)).toMatchObject({ block: false });
		expect(ui.custom).toHaveBeenCalledOnce();
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it("does not retry on UI rejection or abort", async () => {
		const { execute, handler } = await setup();
		const controller = new AbortController();
		const { ctx, ui } = context("tui", controller.signal);
		ui.custom.mockRejectedValueOnce(new Error("UI failed"));
		await expect(execute("id", { command: "fixture" }, undefined, undefined, ctx)).rejects.toThrow(
			"UI failed",
		);
		let resolve: (value: PromptResult) => void = () => {
			throw new Error("missing resolver");
		};
		ui.custom.mockImplementation(
			() =>
				new Promise((done) => {
					resolve = done;
				}),
		);
		const pending = execute("id", { command: "fixture" }, undefined, undefined, ctx);
		await vi.waitFor(() => expect(ui.custom).toHaveBeenCalledTimes(2));
		controller.abort();
		resolve("allow-file-always");
		expect(await pending).toBe(await mocks.execute.mock.results[1]?.value);
		const preAborted = await execute("id", { command: "fixture" }, undefined, undefined, ctx);
		expect(preAborted).toBe(await mocks.execute.mock.results[2]?.value);
		expect(mocks.execute).toHaveBeenCalledTimes(3);
		expect(mocks.violations).toHaveBeenCalledTimes(2);
		const fresh = context();
		expect(await handler(event("read"), fresh.ctx)).toMatchObject({ block: true });
		expect(mocks.write).not.toHaveBeenCalled();
	});

	it("respects an independently aborted tool signal before and after prompting", async () => {
		const { execute, handler } = await setup();
		const controller = new AbortController();
		const { ctx, ui } = context();
		let resolve: (value: PromptResult) => void = () => {
			throw new Error("missing resolver");
		};
		ui.custom.mockImplementation(
			() =>
				new Promise((done) => {
					resolve = done;
				}),
		);
		const pending = execute("id", { command: "fixture" }, controller.signal, undefined, ctx);
		await vi.waitFor(() => expect(ui.custom).toHaveBeenCalledOnce());
		controller.abort();
		resolve("allow-file-always");
		expect(await pending).toBe(await mocks.execute.mock.results[0]?.value);
		expect(await execute("id", { command: "fixture" }, controller.signal, undefined, ctx)).toBe(
			await mocks.execute.mock.results[1]?.value,
		);
		expect(mocks.execute).toHaveBeenCalledTimes(2);
		expect(mocks.violations).toHaveBeenCalledOnce();
		const fresh = context();
		expect(await handler(event("read"), fresh.ctx)).toMatchObject({ block: true });
		expect(mocks.write).not.toHaveBeenCalled();
	});
});
