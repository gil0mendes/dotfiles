import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { isToolCallEventType } from "@earendil-works/pi-coding-agent";
import * as path from "node:path";
import { DEFAULT_CONFIG, loadConfig, totalRules } from "./config";
import { createPathAccessPromptComponent, type PromptResult } from "./prompt";
import { matchPathRules, ruleValue } from "./rules";
import { evaluateBash, outsideCwdPathsInCommand } from "./shell/analysis";
import type { Config } from "./types";
import {
	isGrantTooBroad,
	isOutsidePathAllowed,
	normalizeForDisplay,
	outsideCwdPath,
	persistAllowedOutsidePath,
	storageForm,
} from "./paths";

export default function damageControl(pi: ExtensionAPI) {
	let current = DEFAULT_CONFIG;
	let source: string | null = null;
	const allowedOutsideCwdFiles = new Set<string>();
	const allowedOutsideCwdDirs = new Set<string>();

	const reload = (cwd: string) => {
		const loaded = loadConfig(cwd);
		current = loaded.config;
		source = loaded.source;
		return loaded;
	};

	pi.on("session_start", async (_event, ctx) => {
		const loaded = reload(ctx.cwd);
		const status = `shield: ${totalRules(current)} rules${current.strictMode ? ", strict" : ""}`;
		ctx.ui.setStatus("damage-control", status);
		if (loaded.error)
			ctx.ui.notify(
				`🛡️ Damage Control failed to load ${loaded.source}: ${loaded.error}`,
				"error",
			);
		else
			ctx.ui.notify(
				`🛡️ Damage Control active (${status})${source ? ` from ${source}` : "; no config found"}`,
				"info",
			);
	});

	pi.registerCommand("damage-control-reload", {
		description: "Reload Damage Control rules from settings YAML",
		handler: async (_args, ctx) => {
			const loaded = reload(ctx.cwd);
			ctx.ui.setStatus(
				"damage-control",
				`shield: ${totalRules(current)} rules${current.strictMode ? ", strict" : ""}`,
			);
			ctx.ui.notify(
				loaded.error
					? `Failed: ${loaded.error}`
					: `Reloaded ${totalRules(current)} rules from ${source ?? "defaults"}`,
				loaded.error ? "error" : "info",
			);
		},
	});

	pi.registerCommand("damage-control-status", {
		description: "Show Damage Control rule counts and source",
		handler: async (_args, ctx) => {
			ctx.ui.notify(
				`🛡️ Damage Control: ${totalRules(current)} rules; strictMode=${current.strictMode}; source=${source ?? "none"}`,
				"info",
			);
		},
	});

	pi.on("tool_call", async (event, ctx) => {
		const block = async (reason: string, ask: boolean) => {
			const message = `🛑 BLOCKED by Damage Control: ${reason}\n\nDo not retry with workaround commands, alternate paths, or equivalent destructive actions. Report this block to the user and ask how to proceed.`;
			if (ask) {
				const ok = await ctx.ui.confirm(
					"🛡️ Damage Control confirmation",
					`${reason}\n\nTool: ${event.toolName}\nInput: ${JSON.stringify(event.input).slice(0, 1200)}\n\nAllow once?`,
				);
				if (ok) {
					pi.appendEntry("damage-control-log", {
						action: "allowed_by_user",
						tool: event.toolName,
						input: event.input,
						reason,
						timestamp: Date.now(),
					});
					return { block: false };
				}
			}
			ctx.ui.notify(message, "error");
			ctx.ui.setStatus("damage-control", `blocked: ${reason.slice(0, 40)}`);
			pi.appendEntry("damage-control-log", {
				action: ask ? "denied_by_user" : "blocked",
				tool: event.toolName,
				input: event.input,
				reason,
				timestamp: Date.now(),
			});
			ctx.abort();
			return { block: true, reason: message };
		};

		const applyOutsideCwdPromptResult = (
			result: PromptResult,
			target: string,
			showFileOptions: boolean,
		) => {
			const parentDir = path.dirname(target);
			if (result === "allow-file-once" || result === "allow-dir-once")
				return true;
			if (result === "allow-file-session" || result === "allow-file-always") {
				allowedOutsideCwdFiles.add(target);
				if (result === "allow-file-always")
					persistAllowedOutsidePath(source, storageForm(target, false));
				return true;
			}
			if (result === "allow-dir-session" || result === "allow-dir-always") {
				const dirPath = showFileOptions ? parentDir : target;
				if (isGrantTooBroad(dirPath)) {
					ctx.ui.notify(
						`Cannot grant access to ${normalizeForDisplay(dirPath, ctx.cwd)}/ — too broad. Treating as allow once.`,
						"warning",
					);
					return true;
				}
				allowedOutsideCwdDirs.add(dirPath);
				if (result === "allow-dir-always")
					persistAllowedOutsidePath(source, storageForm(dirPath, true));
				return true;
			}
			return false;
		};

		const askForOutsideCwdPath = async (
			target: string,
			tool: string,
			showFileOptions: boolean,
		) => {
			if (
				!current.askBeforeOutsideCwd ||
				isOutsidePathAllowed(
					target,
					current,
					ctx.cwd,
					allowedOutsideCwdFiles,
					allowedOutsideCwdDirs,
				)
			)
				return { block: false };

			const parentDir = path.dirname(target);
			if (!ctx.hasUI) {
				const reason = `command references path outside current directory: ${target}`;
				return {
					block: true,
					reason: `🛑 BLOCKED by Damage Control: ${reason}`,
				};
			}

			const result = await ctx.ui.custom<PromptResult>(
				createPathAccessPromptComponent(
					tool,
					normalizeForDisplay(target, ctx.cwd),
					normalizeForDisplay(parentDir, ctx.cwd),
					ctx.cwd,
					showFileOptions,
				),
			);
			if (applyOutsideCwdPromptResult(result, target, showFileOptions)) {
				pi.appendEntry("damage-control-log", {
					action: result,
					tool,
					path: target,
					timestamp: Date.now(),
				});
				return { block: false };
			}

			const reason = `user denied access outside current directory: ${target}`;
			ctx.ui.setStatus("damage-control", `blocked: ${reason.slice(0, 40)}`);
			pi.appendEntry("damage-control-log", {
				action: "denied_outside_cwd",
				tool,
				path: target,
				reason,
				timestamp: Date.now(),
			});
			ctx.abort();
			return { block: true, reason: `🛑 BLOCKED by Damage Control: ${reason}` };
		};

		const askForOutsideCwdPaths = async (
			targets: string[],
			tool: string,
			showFileOptions: boolean,
		) => {
			for (const target of targets) {
				const result = await askForOutsideCwdPath(
					target,
					tool,
					showFileOptions,
				);
				if (result.block) return result;
			}
			return { block: false };
		};

		if (isToolCallEventType("bash", event)) {
			const violation = evaluateBash(event.input.command, current);
			if (violation) return block(violation.reason, violation.ask);
			const outside = await askForOutsideCwdPaths(
				outsideCwdPathsInCommand(event.input.command, ctx.cwd),
				event.toolName,
				true,
			);
			if (outside.block) return outside;
		}

		if (
			event.toolName === "multi_tool_use.parallel" &&
			event.input &&
			typeof event.input === "object" &&
			Array.isArray((event.input as any).tool_uses)
		) {
			for (const toolUse of (event.input as any).tool_uses) {
				const name = toolUse.recipient_name ?? toolUse.toolName ?? "";
				const params = toolUse.parameters ?? toolUse.input ?? {};
				if (
					String(name).endsWith(".bash") &&
					typeof params.command === "string"
				) {
					const violation = evaluateBash(params.command, current);
					if (violation) return block(violation.reason, violation.ask);
					const outside = await askForOutsideCwdPaths(
						outsideCwdPathsInCommand(params.command, ctx.cwd),
						String(name),
						true,
					);
					if (outside.block) return outside;
				}
			}
		}

		const input = event.input as Record<string, unknown>;
		const paths = typeof input?.path === "string" ? [input.path] : [];
		if (paths.length > 0) {
			const zero = matchPathRules(paths, current.zeroAccessPaths, ctx.cwd);
			if (zero)
				return block(
					zero.reason ?? `zero-access path: ${ruleValue(zero)}`,
					zero.ask === true,
				);

			if (event.toolName === "write" || event.toolName === "edit") {
				const readOnly = matchPathRules(paths, current.readOnlyPaths, ctx.cwd);
				if (readOnly)
					return block(
						readOnly.reason ?? `read-only path: ${ruleValue(readOnly)}`,
						readOnly.ask === true,
					);
			}

			const outsidePaths = paths
				.map((target) => outsideCwdPath(target, ctx.cwd))
				.filter((target): target is string => Boolean(target));
			const outside = await askForOutsideCwdPaths(
				outsidePaths,
				event.toolName,
				event.toolName !== "ls" && event.toolName !== "find",
			);
			if (outside.block) return outside;
		}

		return { block: false };
	});

	pi.on("user_bash", async (event, ctx) => {
		const violation = evaluateBash(event.command, current);
		if (violation) {
			return {
				result: {
					output: `🛑 BLOCKED by Damage Control: ${violation.reason}`,
					exitCode: 126,
					cancelled: false,
					truncated: false,
				},
			};
		}

		if (!current.askBeforeOutsideCwd) return undefined;
		const outsidePaths = outsideCwdPathsInCommand(
			event.command,
			ctx.cwd,
		).filter(
			(target) =>
				!isOutsidePathAllowed(
					target,
					current,
					ctx.cwd,
					allowedOutsideCwdFiles,
					allowedOutsideCwdDirs,
				),
		);
		if (outsidePaths.length === 0) return undefined;

		for (const target of outsidePaths) {
			if (!ctx.hasUI) {
				return {
					result: {
						output: `🛑 BLOCKED by Damage Control: command references path outside current directory: ${target}`,
						exitCode: 126,
						cancelled: false,
						truncated: false,
					},
				};
			}

			const result = await ctx.ui.custom<PromptResult>(
				createPathAccessPromptComponent(
					"user_bash",
					normalizeForDisplay(target, ctx.cwd),
					normalizeForDisplay(path.dirname(target), ctx.cwd),
					ctx.cwd,
					true,
				),
			);
			if (result === "allow-file-once" || result === "allow-dir-once") continue;
			if (result === "allow-file-session" || result === "allow-file-always") {
				allowedOutsideCwdFiles.add(target);
				if (result === "allow-file-always")
					persistAllowedOutsidePath(source, storageForm(target, false));
				pi.appendEntry("damage-control-log", {
					action: result,
					tool: "user_bash",
					command: event.command,
					path: target,
					timestamp: Date.now(),
				});
				continue;
			}
			if (result === "allow-dir-session" || result === "allow-dir-always") {
				const dirPath = path.dirname(target);
				if (!isGrantTooBroad(dirPath)) {
					allowedOutsideCwdDirs.add(dirPath);
					if (result === "allow-dir-always")
						persistAllowedOutsidePath(source, storageForm(dirPath, true));
				} else {
					ctx.ui.notify(
						`Cannot grant access to ${normalizeForDisplay(dirPath, ctx.cwd)}/ — too broad. Treating as allow once.`,
						"warning",
					);
				}
				pi.appendEntry("damage-control-log", {
					action: result,
					tool: "user_bash",
					command: event.command,
					path: target,
					timestamp: Date.now(),
				});
				continue;
			}

			return {
				result: {
					output: `🛑 BLOCKED by Damage Control: user denied access outside current directory: ${target}`,
					exitCode: 126,
					cancelled: false,
					truncated: false,
				},
			};
		}

		return undefined;
	});
}
