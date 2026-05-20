import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { isToolCallEventType } from "@earendil-works/pi-coding-agent";
import * as path from "node:path";
import { configs } from "./config";
import {
	emitActionBlocked,
	emitFeatureRegister,
	emitRiskDetected,
	type DamageControlBlockSource,
	type DamageControlFeatureId,
} from "./events";
import { createPathAccessPromptComponent, type PromptResult } from "./prompt";
import { matchPathRules, ruleValue } from "./rules";
import { evaluateBash, outsideCwdPathsInCommand } from "./shell/analysis";
import {
	isGrantTooBroad,
	isOutsidePathAllowed,
	normalizeForDisplay,
	outsideCwdPath,
	persistAllowedOutsidePath,
	storageForm,
} from "./paths";

function setupPolicyHook(pi: ExtensionAPI): void {
	const allowedOutsideCwdFiles = new Set<string>();
	const allowedOutsideCwdDirs = new Set<string>();

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
			emitActionBlocked(pi, {
				feature,
				action: { kind: "tool_call", toolName: event.toolName },
				reason,
				block: { source: blockSource, metadata },
				context,
			});
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
					persistAllowedOutsidePath(configs.source, storageForm(target, false));
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
					persistAllowedOutsidePath(configs.source, storageForm(dirPath, true));
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
				!configs.current.askBeforeOutsideCwd ||
				isOutsidePathAllowed(
					target,
					configs.current,
					ctx.cwd,
					allowedOutsideCwdFiles,
					allowedOutsideCwdDirs,
				)
			)
				return { block: false };

			const parentDir = path.dirname(target);
			if (!ctx.hasUI) {
				const reason = `command references path outside current directory: ${target}`;
				emitRiskDetected(pi, {
					feature: "paths",
					risk: { kind: "dangerous", reason, metadata: { path: target } },
					context,
				});
				emitActionBlocked(pi, {
					feature: "paths",
					action: { kind: "tool_call", toolName: tool, path: target },
					reason,
					block: { source: "nonInteractive", metadata: { path: target } },
					context,
				});
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
			emitActionBlocked(pi, {
				feature: "paths",
				action: { kind: "tool_call", toolName: tool, path: target },
				reason,
				block: { source: "user", metadata: { path: target } },
				context,
			});
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
			const violation = evaluateBash(event.input.command, configs.current);
			if (violation)
				return block(violation.reason, violation.ask, "commands", "policy", {
					command: event.input.command,
				});
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
					const violation = evaluateBash(params.command, configs.current);
					if (violation)
						return block(
							violation.reason,
							violation.ask,
							"commands",
							"policy",
							{
								command: params.command,
							},
						);
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
			const zero = matchPathRules(
				paths,
				configs.current.zeroAccessPaths,
				ctx.cwd,
			);
			if (zero)
				return block(
					zero.reason ?? `zero-access path: ${ruleValue(zero)}`,
					zero.ask === true,
					"paths",
					"policy",
					{ path: ruleValue(zero) },
				);

			if (event.toolName === "write" || event.toolName === "edit") {
				const readOnly = matchPathRules(
					paths,
					configs.current.readOnlyPaths,
					ctx.cwd,
				);
				if (readOnly)
					return block(
						readOnly.reason ?? `read-only path: ${ruleValue(readOnly)}`,
						readOnly.ask === true,
						"paths",
						"policy",
						{ path: ruleValue(readOnly) },
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
}

export default function damageControl(pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		const { error: configsError } = configs.load(ctx.cwd);

		const status = `shield: ${configs.totalRules()} rules${configs.current.strictMode ? ", strict" : ""}`;
		ctx.ui.setStatus("damage-control", status);
		for (const feature of ["commands", "paths", "policies"] as const) {
			emitFeatureRegister(pi, feature);
		}
		if (configsError)
			ctx.ui.notify(
				`🛡️ Damage Control failed to load ${configs.source}: ${configsError}`,
				"error",
			);
		else
			ctx.ui.notify(
				`🛡️ Damage Control active (${status})${configs.source ? ` from ${configs.source}` : "; no config found"}`,
				"info",
			);
	});

	pi.registerCommand("damage-control-reload", {
		description: "Reload Damage Control rules from settings YAML",
		handler: async (_args, ctx) => {
			const { error: configsError } = configs.load(ctx.cwd);

			ctx.ui.setStatus(
				"damage-control",
				`shield: ${configs.totalRules()} rules${configs.current.strictMode ? ", strict" : ""}`,
			);
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
				`🛡️ Damage Control: ${configs.totalRules()} rules; strictMode=${configs.current.strictMode}; source=${configs.source ?? "none"}`,
				"info",
			);
		},
	});

	setupPolicyHook(pi);
}
