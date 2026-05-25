import type {
	AgentToolResult,
	BashOperations,
	ExtensionAPI,
	ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import {
	createBashToolDefinition,
	createLocalBashOperations,
	getShellConfig,
	isToolCallEventType,
	SettingsManager,
} from "@earendil-works/pi-coding-agent";
import {
	SandboxManager,
	type SandboxRuntimeConfig,
} from "@carderne/sandbox-runtime";
import * as fs from "node:fs";
import * as path from "node:path";
import { configs, DEFAULT_CONFIG_PATH } from "./config";
import {
	canonicalizePath,
	isGrantTooBroad,
	isWriteDenied,
	normalizeForDisplay,
	shouldPromptForRead,
	shouldPromptForWrite,
	storageForm,
	withSessionAllowances,
} from "./filesystem";
import {
	emitActionBlocked,
	emitFeatureRegister,
	emitRiskDetected,
	type DamageControlBlockSource,
	type DamageControlFeatureId,
} from "./events";
import { createPathAccessPromptComponent, type PromptResult } from "./prompt";
import { evaluateBash } from "./shell/analysis";

type AccessKind = "read" | "write";
type AllowanceScope = "once" | "session" | "always";

type SandboxState = {
	initialized: boolean;
	sessionRead: string[];
	sessionWrite: string[];
};

const sandboxState: SandboxState = {
	initialized: false,
	sessionRead: [],
	sessionWrite: [],
};

function sandboxRuntimeConfig(): SandboxRuntimeConfig {
	return {
		allowBrowserProcess: true,
		network: {
			allowLocalBinding: true,
			allowAllUnixSockets: true,
			allowedDomains: ["*"],
			deniedDomains: [],
		},
		filesystem: withSessionAllowances(
			configs.current.filesystem,
			sandboxState.sessionRead,
			sandboxState.sessionWrite,
		),
		enableWeakerNetworkIsolation: true,
	};
}

async function initializeSandbox(): Promise<void> {
	await SandboxManager.initialize(
		sandboxRuntimeConfig(),
		async () => true,
		true,
	);
	sandboxState.initialized = true;
}

async function reinitializeSandbox(): Promise<void> {
	if (!sandboxState.initialized) return;
	await SandboxManager.reset();
	await initializeSandbox();
}

function createSandboxedBashOperations(shellPath?: string): BashOperations {
	const local = createLocalBashOperations({ shellPath });
	const shell = getShellConfig(shellPath).shell;
	return {
		exec: async (command, cwd, options) => {
			const wrapped = sandboxState.initialized
				? await SandboxManager.wrapWithSandbox(
						command,
						shell,
						undefined,
						options.signal,
					)
				: command;
			return local.exec(wrapped, cwd, options);
		},
	};
}

function appendFilesystemAllowance(
	configPath: string | null,
	kind: AccessKind,
	allowance: string,
): void {
	const targetPath = configPath ?? DEFAULT_CONFIG_PATH;
	fs.mkdirSync(path.dirname(targetPath), { recursive: true });
	const currentText = fs.existsSync(targetPath)
		? fs.readFileSync(targetPath, "utf8")
		: "";
	if (
		currentText
			.split(/\r?\n/)
			.some(
				(line) =>
					line.trim() === `- ${allowance}` ||
					line.trim() === `- '${allowance}'` ||
					line.trim() === `- "${allowance}"`,
			)
	)
		return;

	const key = kind === "read" ? "allowRead" : "allowWrite";
	const lines = currentText.trimEnd().split(/\r?\n/);
	const filesystemIndex = lines.findIndex((line) =>
		/^filesystem:\s*$/.test(line),
	);
	if (filesystemIndex === -1) {
		lines.push("", "filesystem:", `  ${key}:`, `    - ${allowance}`);
		fs.writeFileSync(targetPath, `${lines.join("\n")}\n`, "utf8");
		return;
	}

	const keyIndex = lines.findIndex(
		(line, index) =>
			index > filesystemIndex && new RegExp(`^  ${key}:\\s*$`).test(line),
	);
	if (keyIndex === -1) {
		lines.splice(filesystemIndex + 1, 0, `  ${key}:`, `    - ${allowance}`);
		fs.writeFileSync(targetPath, `${lines.join("\n")}\n`, "utf8");
		return;
	}

	let insertAt = keyIndex + 1;
	while (insertAt < lines.length && /^    - /.test(lines[insertAt] ?? ""))
		insertAt++;
	lines.splice(insertAt, 0, `    - ${allowance}`);
	fs.writeFileSync(targetPath, `${lines.join("\n")}\n`, "utf8");
}

function promptScope(result: PromptResult): AllowanceScope | null {
	if (result.endsWith("once")) return "once";
	if (result.endsWith("session")) return "session";
	if (result.endsWith("always")) return "always";
	return null;
}

function promptIsDirectory(result: PromptResult): boolean {
	return result.includes("dir");
}

async function promptForFilesystemAccess(
	ctx: ExtensionContext,
	kind: AccessKind,
	tool: string,
	target: string,
	showFileOptions: boolean,
	grantOnceForSandboxRetry = false,
): Promise<boolean> {
	if (!ctx.hasUI) return false;
	const parentDir = path.dirname(target);
	const result = await ctx.ui.custom<PromptResult>(
		createPathAccessPromptComponent(
			`${tool} ${kind}`,
			normalizeForDisplay(target, ctx.cwd),
			normalizeForDisplay(parentDir, ctx.cwd),
			ctx.cwd,
			showFileOptions,
		),
	);
	const scope = promptScope(result);
	if (!scope) return false;

	const grantTarget =
		promptIsDirectory(result) && showFileOptions ? parentDir : target;
	if (promptIsDirectory(result) && isGrantTooBroad(grantTarget, ctx.cwd)) {
		ctx.ui.notify(
			`Cannot grant access to ${normalizeForDisplay(grantTarget, ctx.cwd)}/ — too broad. Treating as allow once.`,
			"warning",
		);
		return true;
	}

	const allowance = storageForm(
		grantTarget,
		ctx.cwd,
		promptIsDirectory(result),
	);
	if (scope === "session" || scope === "always" || grantOnceForSandboxRetry) {
		const session =
			kind === "read" ? sandboxState.sessionRead : sandboxState.sessionWrite;
		if (!session.includes(allowance)) session.push(allowance);
	}
	if (scope === "always")
		appendFilesystemAllowance(configs.source, kind, allowance);
	await reinitializeSandbox();
	return true;
}

function setupPolicyHook(pi: ExtensionAPI): void {
	pi.on("tool_call", async (event, ctx) => {
		const context = {
			toolName: event.toolName,
			input:
				event.input && typeof event.input === "object"
					? (event.input as Record<string, unknown>)
					: undefined,
		};
		const block = async (
			reason: string,
			ask: boolean,
			feature: DamageControlFeatureId = "policies",
			blockSource: DamageControlBlockSource = ask ? "user" : "policy",
			metadata?: Record<string, unknown>,
		) => {
			emitRiskDetected(pi, {
				feature,
				risk: { kind: "dangerous", reason, metadata },
				context,
			});
			const message = `🛑 BLOCKED by Damage Control: ${reason}\n\nDo not retry with workaround commands, alternate paths, or equivalent destructive actions. Report this block to the user and ask how to proceed.`;
			if (ask) {
				const ok = await ctx.ui.confirm(
					"🛡️ Damage Control confirmation",
					`${reason}\n\nTool: ${event.toolName}\nInput: ${JSON.stringify(event.input).slice(0, 1200)}\n\nAllow once?`,
				);
				if (ok) return { block: false };
			}
			emitActionBlocked(pi, {
				feature,
				action: { kind: "tool_call", toolName: event.toolName },
				reason,
				block: { source: blockSource, metadata },
				context,
			});
			ctx.ui.notify(message, "error");
			ctx.ui.setStatus("damage-control", `blocked: ${reason.slice(0, 40)}`);
			ctx.abort();
			return { block: true, reason: message };
		};

		if (isToolCallEventType("bash", event)) {
			const violation = evaluateBash(event.input.command, configs.current);
			if (violation)
				return block(violation.reason, violation.ask, "commands", "policy", {
					command: event.input.command,
				});
		}

		if (
			event.toolName === "read" &&
			context.input?.path &&
			typeof context.input.path === "string"
		) {
			const target = canonicalizePath(context.input.path, ctx.cwd);
			if (
				shouldPromptForRead(target, sandboxRuntimeConfig().filesystem, ctx.cwd)
			) {
				const allowed = await promptForFilesystemAccess(
					ctx,
					"read",
					event.toolName,
					target,
					true,
				);
				if (!allowed)
					return block(
						`read access denied for ${target}`,
						false,
						"paths",
						"user",
						{
							path: target,
						},
					);
			}
		}

		if (
			(event.toolName === "write" || event.toolName === "edit") &&
			context.input?.path &&
			typeof context.input.path === "string"
		) {
			const target = canonicalizePath(context.input.path, ctx.cwd);
			const filesystem = sandboxRuntimeConfig().filesystem;
			if (isWriteDenied(target, filesystem, ctx.cwd))
				return block(
					`write access denied by denyWrite for ${target}`,
					false,
					"paths",
					"policy",
					{
						path: target,
					},
				);
			if (shouldPromptForWrite(target, filesystem, ctx.cwd)) {
				const allowed = await promptForFilesystemAccess(
					ctx,
					"write",
					event.toolName,
					target,
					true,
				);
				if (!allowed)
					return block(
						`write access denied for ${target}`,
						false,
						"paths",
						"user",
						{
							path: target,
						},
					);
			}
		}

		return { block: false };
	});
}

function statusText(): string {
	return `shield: ${configs.totalRules()} rules${configs.current.strictMode ? ", strict" : ""}`;
}

function setupSessionStart(pi: ExtensionAPI): void {
	pi.on("session_start", async (_event, ctx) => {
		const { error: configsError } = configs.load(ctx.cwd);
		ctx.ui.setStatus("damage-control", statusText());
		for (const feature of ["commands", "paths", "policies"] as const) {
			emitFeatureRegister(pi, feature);
		}
		if (configsError) {
			ctx.ui.notify(
				`🛡️ Damage Control failed to load ${configs.source}: ${configsError}`,
				"error",
			);
			return;
		}
		if (process.platform !== "darwin" && process.platform !== "linux") {
			ctx.ui.notify(
				`🛡️ Damage Control filesystem sandbox is not supported on ${process.platform}`,
				"warning",
			);
			return;
		}
		try {
			await initializeSandbox();
			ctx.ui.notify(
				`🛡️ Damage Control active (${statusText()})${configs.source ? ` from ${configs.source}` : "; no config found"}`,
				"info",
			);
		} catch (error) {
			sandboxState.initialized = false;
			ctx.ui.notify(
				`🛡️ Damage Control sandbox failed: ${error instanceof Error ? error.message : String(error)}`,
				"error",
			);
		}
	});
}

function registerCommands(pi: ExtensionAPI): void {
	pi.registerCommand("damage-control-reload", {
		description: "Reload Damage Control rules from settings YAML",
		handler: async (_args, ctx) => {
			const { error: configsError } = configs.load(ctx.cwd);
			await reinitializeSandbox();
			ctx.ui.setStatus("damage-control", statusText());
			ctx.ui.notify(
				configsError
					? `Failed: ${configsError}`
					: `Reloaded ${configs.totalRules()} rules from ${configs.source ?? "defaults"}`,
				configsError ? "error" : "info",
			);
		},
	});

	pi.registerCommand("damage-control-status", {
		description: "Show Damage Control rule counts and source",
		handler: async (_args, ctx) => {
			ctx.ui.notify(
				`🛡️ Damage Control: ${configs.totalRules()} rules; strictMode=${configs.current.strictMode}; sandbox=${sandboxState.initialized}; source=${configs.source ?? "none"}`,
				"info",
			);
		},
	});
}

function resultText<TDetails>(result: AgentToolResult<TDetails>): string {
	return result.content
		.filter((content) => content.type === "text")
		.map((content) => content.text)
		.join("\n");
}

type SandboxBlockedAccess = {
	kind: AccessKind;
	path: string;
};

const READ_ERROR_COMMANDS = new Set([
	"awk",
	"cat",
	"find",
	"grep",
	"head",
	"less",
	"ls",
	"more",
	"rg",
	"sed",
	"stat",
	"tail",
]);

const SHELL_ERROR_COMMANDS = new Set(["bash", "dash", "fish", "sh", "zsh"]);

function parseSandboxViolation(line: string): SandboxBlockedAccess | null {
	const match = line.match(/\b(file-read|file-write)[^\s]*\s+(\/\S+)/);
	if (!match) return null;
	return { kind: match[1] === "file-read" ? "read" : "write", path: match[2] };
}

function classifyOperationNotPermitted(
	line: string,
	cwd: string,
): SandboxBlockedAccess | null {
	const match = line.match(
		/^(?:(.+?):\s+)?(\/[^\s:]+): Operation not permitted$/,
	);
	if (!match) return null;
	const commandName = path.basename((match[1] ?? "").trim());
	const target = canonicalizePath(match[2], cwd);
	const filesystem = sandboxRuntimeConfig().filesystem;
	if (READ_ERROR_COMMANDS.has(commandName))
		return { kind: "read", path: target };
	if (SHELL_ERROR_COMMANDS.has(commandName))
		return { kind: "write", path: target };
	if (shouldPromptForRead(target, filesystem, cwd))
		return { kind: "read", path: target };
	return { kind: "write", path: target };
}

export function extractSandboxBlockedAccesses(
	output: string,
	violationLines: readonly string[],
	cwd: string,
): SandboxBlockedAccess[] {
	const accesses = new Map<string, SandboxBlockedAccess>();
	const addAccess = (access: SandboxBlockedAccess | null) => {
		if (!access) return;
		const target = canonicalizePath(access.path, cwd);
		accesses.set(`${access.kind}:${target}`, { ...access, path: target });
	};

	for (const line of violationLines) addAccess(parseSandboxViolation(line));
	for (const line of output.split(/\r?\n/))
		addAccess(classifyOperationNotPermitted(line.trim(), cwd));

	return [...accesses.values()];
}

function registerSandboxedBash(pi: ExtensionAPI): void {
	const localCwd = process.cwd();
	const shellPath = SettingsManager.create(localCwd).getShellPath();
	const bashTool = createBashToolDefinition(localCwd, {
		shellPath,
		operations: createSandboxedBashOperations(shellPath),
	});
	pi.registerTool({
		...bashTool,
		label: "bash (damage-control sandbox)",
		execute: async (id, params, signal, onUpdate, ctx) => {
			const startedAt = new Date();
			const result = await bashTool.execute(id, params, signal, onUpdate, ctx);
			if (!ctx.hasUI) return result;

			const violationLines = SandboxManager.getSandboxViolationStore()
				.getViolationsForCommand(params.command)
				.filter((violation) => violation.timestamp >= startedAt)
				.map((violation) => violation.line);
			const blockedAccesses = extractSandboxBlockedAccesses(
				resultText(result),
				violationLines,
				ctx.cwd,
			);
			if (blockedAccesses.length === 0) return result;

			for (const access of blockedAccesses) {
				const allowed = await promptForFilesystemAccess(
					ctx,
					access.kind,
					"bash",
					access.path,
					true,
					true,
				);
				if (!allowed) return result;
			}

			onUpdate?.({
				content: [
					{
						type: "text",
						text: `\n--- Filesystem access granted; retrying command ---\n`,
					},
				],
				details: undefined,
			});
			return bashTool.execute(id, params, signal, onUpdate, ctx);
		},
	});

	pi.on("user_bash", () => {
		if (!sandboxState.initialized) return;
		return { operations: createSandboxedBashOperations(shellPath) };
	});
}

export default function damageControl(pi: ExtensionAPI) {
	setupSessionStart(pi);
	registerCommands(pi);
	registerSandboxedBash(pi);
	setupPolicyHook(pi);
	pi.on("session_shutdown", async () => {
		if (!sandboxState.initialized) return;
		await SandboxManager.reset();
		sandboxState.initialized = false;
	});
}
