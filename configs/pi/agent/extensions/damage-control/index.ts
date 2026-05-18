import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { isToolCallEventType } from "@earendil-works/pi-coding-agent";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { createPathAccessPromptComponent, type PromptResult } from "./prompt";

type Rule = {
	pattern?: string;
	path?: string;
	command?: string;
	reason?: string;
	ask?: boolean;
};

type Config = {
	strictMode?: boolean;
	/** Ask before tools reference paths outside ctx.cwd. */
	askBeforeOutsideCwd?: boolean;
	allowedOutsideCwdPaths: string[];
	bashToolPatterns: Rule[];
	zeroAccessPaths: Rule[];
	readOnlyPaths: Rule[];
	noDeletePaths: Rule[];
	strictModeAllowedList: Rule[];
};

const DEFAULT_CONFIG: Config = {
	strictMode: false,
	askBeforeOutsideCwd: false,
	allowedOutsideCwdPaths: [],
	bashToolPatterns: [],
	zeroAccessPaths: [],
	readOnlyPaths: [],
	noDeletePaths: [],
	strictModeAllowedList: [],
};

const DEFAULT_CONFIG_PATH = path.join(
	os.homedir(),
	".pi",
	"agent",
	"settings",
	"damage-control-rules.yaml",
);

const CONFIG_PATHS = [
	DEFAULT_CONFIG_PATH,
	path.join(os.homedir(), ".pi", "damage-control-rules.yaml"),
];

function stripQuotes(value: string): string {
	const trimmed = value.trim();
	if (
		(trimmed.startsWith('"') && trimmed.endsWith('"')) ||
		(trimmed.startsWith("'") && trimmed.endsWith("'"))
	) {
		return trimmed.slice(1, -1);
	}
	return trimmed;
}

function parseScalar(value: string): string | boolean {
	const unquoted = stripQuotes(value);
	if (unquoted === "true") return true;
	if (unquoted === "false") return false;
	return unquoted;
}

function removeComment(line: string): string {
	let quote: string | null = null;
	for (let i = 0; i < line.length; i++) {
		const ch = line[i] ?? "";
		if ((ch === '"' || ch === "'") && line[i - 1] !== "\\")
			quote = quote === ch ? null : (quote ?? ch);
		if (ch === "#" && !quote && (i === 0 || /\s/.test(line[i - 1] ?? "")))
			return line.slice(0, i);
	}
	return line;
}

function parseSimpleYaml(input: string): Partial<Config> {
	const out: Record<string, unknown> = {};
	let section: string | null = null;
	let currentItem: Record<string, unknown> | null = null;

	for (const rawLine of input.split(/\r?\n/)) {
		const line = removeComment(rawLine).replace(/\t/g, "  ").trimEnd();
		if (!line.trim()) continue;

		const top = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
		if (top && top[1] && top[2] !== undefined && !rawLine.startsWith(" ")) {
			section = top[1];
			const rest = top[2].trim();
			if (rest === "") out[section] = [];
			else out[section] = parseScalar(rest);
			currentItem = null;
			continue;
		}

		if (!section) continue;
		const sectionName = section;
		const arr = Array.isArray(out[sectionName])
			? (out[sectionName] as unknown[])
			: (out[sectionName] = [] as unknown[]);
		const item = line.match(/^\s*-\s*(.*)$/);
		if (item && item[1] !== undefined) {
			const rest = item[1].trim();
			const kv = rest.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
			if (kv && kv[1] && kv[2] !== undefined) {
				currentItem = { [kv[1]]: parseScalar(kv[2]) };
				arr.push(currentItem);
			} else if (section === "allowedOutsideCwdPaths") {
				arr.push(parseScalar(rest));
				currentItem = null;
			} else {
				currentItem = { pattern: parseScalar(rest) };
				arr.push(currentItem);
			}
			continue;
		}

		const nested = line.match(/^\s+([A-Za-z0-9_-]+):\s*(.*)$/);
		if (nested && nested[1] && nested[2] !== undefined && currentItem)
			currentItem[nested[1]] = parseScalar(nested[2]);
	}

	return out as Partial<Config>;
}

function normalizeRule(
	raw: unknown,
	defaultKey: "pattern" | "path" | "command",
): Rule | null {
	if (typeof raw === "string")
		return { [defaultKey]: raw, reason: `${defaultKey} ${raw} is protected` };
	if (!raw || typeof raw !== "object") return null;
	const rule = raw as Record<string, unknown>;
	const value = rule.pattern ?? rule.path ?? rule.command;
	if (typeof value !== "string") return null;
	return {
		pattern: typeof rule.pattern === "string" ? rule.pattern : undefined,
		path: typeof rule.path === "string" ? rule.path : undefined,
		command: typeof rule.command === "string" ? rule.command : undefined,
		reason:
			typeof rule.reason === "string"
				? rule.reason
				: `${defaultKey} ${value} is protected`,
		ask: rule.ask === true,
	};
}

function normalizeConfig(loaded: Partial<Config>): Config {
	const normalize = (
		key: keyof Config,
		defaultKey: "pattern" | "path" | "command",
	) =>
		(Array.isArray(loaded[key]) ? (loaded[key] as unknown[]) : [])
			.map((r) => normalizeRule(r, defaultKey))
			.filter((r): r is Rule => Boolean(r));

	const normalizeStrings = (key: keyof Config) =>
		(Array.isArray(loaded[key]) ? (loaded[key] as unknown[]) : []).filter(
			(v): v is string => typeof v === "string",
		);

	return {
		strictMode: loaded.strictMode === true,
		askBeforeOutsideCwd: loaded.askBeforeOutsideCwd === true,
		allowedOutsideCwdPaths: normalizeStrings("allowedOutsideCwdPaths"),
		bashToolPatterns: normalize("bashToolPatterns", "pattern"),
		zeroAccessPaths: normalize("zeroAccessPaths", "path"),
		readOnlyPaths: normalize("readOnlyPaths", "path"),
		noDeletePaths: normalize("noDeletePaths", "path"),
		strictModeAllowedList: normalize("strictModeAllowedList", "command"),
	};
}

function findConfigPath(cwd: string): string | null {
	const projectPath = path.join(cwd, ".pi", "damage-control-rules.yaml");
	if (fs.existsSync(projectPath)) return projectPath;
	return CONFIG_PATHS.find((p) => fs.existsSync(p)) ?? null;
}

function loadConfig(cwd: string): {
	config: Config;
	source: string | null;
	error?: string;
} {
	const source = findConfigPath(cwd);
	if (!source) return { config: DEFAULT_CONFIG, source: null };
	try {
		return {
			config: normalizeConfig(parseSimpleYaml(fs.readFileSync(source, "utf8"))),
			source,
		};
	} catch (error) {
		return {
			config: DEFAULT_CONFIG,
			source,
			error: error instanceof Error ? error.message : String(error),
		};
	}
}

function expandHome(p: string): string {
	return p === "~" || p.startsWith("~/")
		? path.join(os.homedir(), p.slice(2))
		: p;
}

function absoluteTarget(p: string, cwd: string): string {
	return path.resolve(cwd, expandHome(p));
}

function globToRegex(glob: string): RegExp {
	const expanded = expandHome(glob).replace(/\/+$|\\+$/g, "");
	const escaped = expanded
		.replace(/[.+^${}()|[\]\\]/g, "\\$&")
		.replace(/\*/g, "[^/]*");
	return new RegExp(`(^|/)${escaped}($|/)`);
}

function pathMatches(target: string, rulePath: string, cwd: string): boolean {
	const absTarget = absoluteTarget(target, cwd).replace(/\/+$|\\+$/g, "");
	const relTarget = path.relative(cwd, absTarget) || ".";
	const expandedRule = expandHome(rulePath);

	if (expandedRule.endsWith("/")) {
		const absRule = (
			path.isAbsolute(expandedRule)
				? expandedRule
				: path.resolve(cwd, expandedRule)
		).replace(/\/+$|\\+$/g, "");
		return absTarget === absRule || absTarget.startsWith(absRule + path.sep);
	}

	const regex = globToRegex(expandedRule);
	return regex.test(absTarget) || regex.test(relTarget);
}

function ruleValue(rule: Rule): string {
	return rule.pattern ?? rule.path ?? rule.command ?? "";
}

function matchPathRules(
	targets: string[],
	rules: Rule[],
	cwd: string,
): Rule | null {
	for (const target of targets) {
		for (const rule of rules) {
			const value = ruleValue(rule);
			if (value && pathMatches(target, value, cwd)) return rule;
		}
	}
	return null;
}

function matchRegexRules(text: string, rules: Rule[]): Rule | null {
	for (const rule of rules) {
		const value = ruleValue(rule);
		if (!value) continue;
		try {
			if (new RegExp(value, "im").test(text)) return rule;
		} catch {
			if (text.includes(value)) return rule;
		}
	}
	return null;
}

function matchLiteralPathInCommand(
	command: string,
	rules: Rule[],
): Rule | null {
	for (const rule of rules) {
		const value = ruleValue(rule);
		if (!value) continue;
		const bare = value
			.replace(/^~\//, "")
			.replace(/\/+$|\\+$/g, "")
			.replace(/^\*+/, "")
			.replace(/\*+$/, "");
		if (bare && command.includes(bare)) return rule;
	}
	return null;
}

function commandLooksMutating(command: string): boolean {
	return (
		/(^|[;&|]\s*)(rm|rmdir|mv|cp|install|touch|mkdir|chmod|chown|truncate|tee|sed\s+-i|perl\s+-pi|python\b|node\b|bun\b|npm\b|pnpm\b|yarn\b|git\s+(clean|reset|checkout|restore|branch|push)|cat\s*>|echo\s+.*>|printf\s+.*>)\b/im.test(
			command,
		) || />{1,2}/.test(command)
	);
}

function commandLooksDeleting(command: string): boolean {
	return /(^|[;&|]\s*)(rm|rmdir|unlink|shred|trash|mv|git\s+(clean|branch\s+(-\S*)*D|push\s+\S+\s+--delete))\b/im.test(
		command,
	);
}

function splitCommandWords(command: string): string[] {
	const words: string[] = [];
	let current = "";
	let quote: string | null = null;
	let escaped = false;

	for (const ch of command) {
		if (escaped) {
			current += ch;
			escaped = false;
			continue;
		}
		if (ch === "\\") {
			escaped = true;
			continue;
		}
		if (quote) {
			if (ch === quote) quote = null;
			else current += ch;
			continue;
		}
		if (ch === '"' || ch === "'") {
			quote = ch;
			continue;
		}
		if (/\s|[;&|()]/.test(ch)) {
			if (current) words.push(current);
			current = "";
			continue;
		}
		current += ch;
	}
	if (current) words.push(current);
	return words;
}

function cleanCommandPathToken(word: string): string | null {
	const withoutRedirect = word.replace(/^[<>]+/, "").replace(/[,:;]+$/, "");
	if (
		!withoutRedirect ||
		withoutRedirect.startsWith("-") ||
		/^[A-Za-z_][A-Za-z0-9_]*=/.test(withoutRedirect)
	)
		return null;
	if (/^[a-z][a-z0-9+.-]+:\/\//i.test(withoutRedirect)) return null;
	if (
		/^\.\.?($|\/)/.test(withoutRedirect) ||
		withoutRedirect.startsWith("~/") ||
		withoutRedirect.startsWith("/") ||
		withoutRedirect.includes("/")
	)
		return withoutRedirect;
	return null;
}

function isInsideOrSame(target: string, root: string): boolean {
	const rel = path.relative(root, target);
	return rel === "" || (!rel.startsWith("..") && !path.isAbsolute(rel));
}

function normalizeForDisplay(target: string, cwd: string): string {
	const home = os.homedir();
	if (target === home) return "~";
	if (target.startsWith(home + path.sep))
		return `~${target.slice(home.length)}`;
	const rel = path.relative(cwd, target);
	return rel && !rel.startsWith("..") && !path.isAbsolute(rel) ? rel : target;
}

function outsideCwdPath(target: string, cwd: string): string | null {
	const cwdAbs = path.resolve(cwd);
	const cwdReal = fs.realpathSync.native(cwd);
	const resolved = absoluteTarget(target, cwd);
	const normalized = fs.existsSync(resolved)
		? fs.realpathSync.native(resolved)
		: path.normalize(resolved);
	if (
		isInsideOrSame(path.normalize(resolved), cwdAbs) ||
		isInsideOrSame(normalized, cwdReal)
	)
		return null;
	return normalized;
}

function storageForm(target: string, isDirectory: boolean): string {
	const home = os.homedir();
	const normalized = target.replace(/\/+$|\\+$/g, "");
	const display =
		normalized === home
			? "~"
			: normalized.startsWith(home + path.sep)
				? `~${normalized.slice(home.length)}`
				: normalized;
	return isDirectory ? `${display}/` : display;
}

function isGrantTooBroad(target: string): boolean {
	const normalized = target.replace(/\/+$|\\+$/g, "");
	return normalized === "/" || normalized === os.homedir();
}

function allowedOutsideRules(config: Config): Rule[] {
	return config.allowedOutsideCwdPaths.map((allowedPath) => ({
		path: allowedPath,
	}));
}

function isOutsidePathAllowed(
	target: string,
	config: Config,
	cwd: string,
	sessionFiles: Set<string>,
	sessionDirs: Set<string>,
): boolean {
	if (
		sessionFiles.has(target) ||
		[...sessionDirs].some((root) => isInsideOrSame(target, root))
	)
		return true;
	return Boolean(matchPathRules([target], allowedOutsideRules(config), cwd));
}

function persistAllowedOutsidePath(
	configPath: string | null,
	storagePath: string,
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
					line.trim() === `- ${storagePath}` ||
					line.trim() === `- '${storagePath}'` ||
					line.trim() === `- \"${storagePath}\"`,
			)
	)
		return;
	const item = `  - '${storagePath.replace(/'/g, "''")}'\n`;
	const section = currentText.match(/^allowedOutsideCwdPaths:\s*$/m);
	if (!section || section.index === undefined) {
		const prefix =
			currentText.endsWith("\n") || currentText.length === 0
				? currentText
				: `${currentText}\n`;
		fs.writeFileSync(targetPath, `${prefix}\nallowedOutsideCwdPaths:\n${item}`);
		return;
	}

	const afterSection = currentText.slice(section.index + section[0].length);
	const nextTopLevel = afterSection.search(/\n[A-Za-z0-9_-]+:\s*/);
	const insertAt =
		nextTopLevel === -1
			? currentText.length
			: section.index + section[0].length + nextTopLevel;
	const before = currentText.slice(0, insertAt);
	const after = currentText.slice(insertAt);
	fs.writeFileSync(
		targetPath,
		`${before.endsWith("\n") ? before : `${before}\n`}${item}${after}`,
	);
}

function outsideCwdPathsInCommand(command: string, cwd: string): string[] {
	const cwdAbs = path.resolve(cwd);
	const cwdReal = fs.realpathSync.native(cwd);
	const paths = new Set<string>();

	for (const word of splitCommandWords(command)) {
		const token = cleanCommandPathToken(word);
		if (!token) continue;
		const resolved = absoluteTarget(token, cwd);
		const normalized = fs.existsSync(resolved)
			? fs.realpathSync.native(resolved)
			: path.normalize(resolved);
		if (
			!isInsideOrSame(path.normalize(resolved), cwdAbs) &&
			!isInsideOrSame(normalized, cwdReal)
		)
			paths.add(normalized);
	}

	return [...paths];
}

function evaluateBash(
	command: string,
	config: Config,
): { rule?: Rule; reason: string; ask: boolean } | null {
	const explicit = matchRegexRules(command, config.bashToolPatterns);
	if (explicit)
		return {
			rule: explicit,
			reason: explicit.reason ?? "dangerous bash command",
			ask: explicit.ask === true,
		};

	const zero = matchLiteralPathInCommand(command, config.zeroAccessPaths);
	if (zero)
		return {
			rule: zero,
			reason:
				zero.reason ?? `bash references zero-access path: ${ruleValue(zero)}`,
			ask: zero.ask === true,
		};

	if (commandLooksMutating(command)) {
		const readOnly = matchLiteralPathInCommand(command, config.readOnlyPaths);
		if (readOnly)
			return {
				rule: readOnly,
				reason:
					readOnly.reason ??
					`bash may modify read-only path: ${ruleValue(readOnly)}`,
				ask: readOnly.ask === true,
			};
	}

	if (commandLooksDeleting(command)) {
		const noDelete = matchLiteralPathInCommand(command, config.noDeletePaths);
		if (noDelete)
			return {
				rule: noDelete,
				reason:
					noDelete.reason ??
					`bash may delete protected path: ${ruleValue(noDelete)}`,
				ask: noDelete.ask === true,
			};
	}

	if (config.strictMode) {
		const allowed = matchRegexRules(command, config.strictModeAllowedList);
		if (!allowed)
			return {
				reason:
					"strict mode blocks bash commands not matching strictModeAllowedList",
				ask: false,
			};
	}

	return null;
}

function totalRules(config: Config): number {
	return (
		config.bashToolPatterns.length +
		config.zeroAccessPaths.length +
		config.readOnlyPaths.length +
		config.noDeletePaths.length +
		config.strictModeAllowedList.length
	);
}

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
