import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { FilesystemGateConfig } from "./types";

export function isInsideOrSame(target: string, root: string): boolean {
	const rel = path.relative(root, target);
	return rel === "" || (!rel.startsWith("..") && !path.isAbsolute(rel));
}

export function normalizeForDisplay(target: string, cwd: string): string {
	const home = os.homedir();
	if (target === home) return "~";
	if (target.startsWith(home + path.sep))
		return `~${target.slice(home.length)}`;
	const rel = path.relative(cwd, target);
	return rel && !rel.startsWith("..") && !path.isAbsolute(rel) ? rel : target;
}

export const DEFAULT_FILESYSTEM_CONFIG: FilesystemGateConfig = {
	denyRead: [
		"/",
		"/Users",
		".env",
		".env.*",
		"*.env",
		"~/.ssh/",
		"~/.gnupg/",
		"~/.aws/",
		"~/.config/gcloud/",
		"~/.azure/",
		"~/.kube/",
		"~/.docker/",
		"~/.netrc",
		"~/.npmrc",
		"~/.pypirc",
		"~/.git-credentials",
		"*-credentials.json",
		"*serviceAccount*.json",
		"*service-account*.json",
		"*.pem",
		"*.key",
		"*.p12",
		"*.pfx",
		"*.tfstate",
		"*.tfstate.backup",
		".terraform/",
		".supabase/",
		"dump.sql",
		"backup.sql",
		"*.dump",
	],
	allowRead: [
		".",
		"/etc/",
		"/usr/",
		"/bin/",
		"/sbin/",
		"/boot/",
		"/root/",
		"~/projects",
		"~/.config",
		"~/.cargo",
		"~/.local",
		"~/Library",
		"~/.cache",
		"~/.bash_history",
		"~/.zsh_history",
		"~/.bashrc",
		"~/.zshrc",
		"~/.profile",
		"package-lock.json",
		"yarn.lock",
		"pnpm-lock.yaml",
		"bun.lockb",
		"*.lock",
		"*.min.js",
		"*.min.css",
		"dist/",
		"build/",
		".next/",
		".nuxt/",
		"__pycache__/",
		".venv/",
		"venv/",
		"target/",
	],
	allowWrite: [".", "/tmp", "~/.pi/", "~/.cache/uv", "~/.rustup", "~/.agent-browser"],
	denyWrite: [".env", ".env.*", "*.pem", "*.key"],
};

export type FilesystemAccessKind = "read" | "write";

export function mergeFilesystemConfig(
	base: FilesystemGateConfig,
	override?: Partial<FilesystemGateConfig>,
): FilesystemGateConfig {
	return {
		denyRead: override?.denyRead ?? base.denyRead,
		allowRead: override?.allowRead ?? base.allowRead,
		allowWrite: override?.allowWrite ?? base.allowWrite,
		denyWrite: override?.denyWrite ?? base.denyWrite,
		allowGitConfig: override?.allowGitConfig ?? base.allowGitConfig,
	};
}

export function withSessionAllowances(
	config: FilesystemGateConfig,
	sessionRead: readonly string[],
	sessionWrite: readonly string[],
): FilesystemGateConfig {
	return {
		...config,
		allowRead: [...(config.allowRead ?? []), ...sessionRead],
		allowWrite: [...config.allowWrite, ...sessionWrite],
	};
}

function expandPattern(pattern: string, cwd: string): string {
	const expanded =
		pattern === "~" || pattern.startsWith("~/")
			? path.join(os.homedir(), pattern.slice(2))
			: pattern;
	return path.isAbsolute(expanded) ? path.resolve(expanded) : path.resolve(cwd, expanded);
}

export function canonicalizePath(target: string, cwd: string): string {
	const absolute = expandPattern(target, cwd);
	try {
		return fs.realpathSync.native(absolute);
	} catch {
		const tail: string[] = [];
		let probe = absolute;
		while (!fs.existsSync(probe)) {
			const parent = path.dirname(probe);
			if (parent === probe) return absolute;
			tail.unshift(path.basename(probe));
			probe = parent;
		}
		try {
			return path.resolve(fs.realpathSync.native(probe), ...tail);
		} catch {
			return absolute;
		}
	}
}

function canonicalizeGlobPattern(pattern: string, cwd: string): string {
	const absolutePattern = expandPattern(pattern, cwd);
	const wildcardIndex = absolutePattern.indexOf("*");
	const prefix = absolutePattern.slice(0, wildcardIndex);
	const baseDir = path.dirname(prefix);
	return `${canonicalizePath(baseDir, cwd)}${absolutePattern.slice(baseDir.length)}`;
}

function patternMatches(target: string, pattern: string, cwd: string): boolean {
	if (pattern.includes("*")) {
		const escaped = canonicalizeGlobPattern(pattern, cwd)
			.replace(/[.+^${}()|[\]\\]/g, "\\$&")
			.replace(/\*/g, ".*");
		return new RegExp(`^${escaped}$`).test(target);
	}

	const canonicalPattern = canonicalizePath(pattern, cwd).replace(/\/+$/g, "");
	const normalizedTarget = target.replace(/\/+$/g, "");
	return (
		normalizedTarget === canonicalPattern ||
		normalizedTarget.startsWith(`${canonicalPattern}${path.sep}`)
	);
}

export function matchesFilesystemPattern(
	target: string,
	patterns: readonly string[] | undefined,
	cwd: string,
): boolean {
	if (!patterns || patterns.length === 0) return false;
	const canonical = canonicalizePath(target, cwd);
	return patterns.some((pattern) => patternMatches(canonical, pattern, cwd));
}

export function shouldPromptForRead(
	target: string,
	filesystem: FilesystemGateConfig,
	cwd: string,
): boolean {
	return (
		matchesFilesystemPattern(target, filesystem.denyRead, cwd) &&
		!matchesFilesystemPattern(target, filesystem.allowRead, cwd)
	);
}

export function shouldPromptForWrite(
	target: string,
	filesystem: FilesystemGateConfig,
	cwd: string,
): boolean {
	return !matchesFilesystemPattern(target, filesystem.allowWrite, cwd);
}

export function isWriteDenied(
	target: string,
	filesystem: FilesystemGateConfig,
	cwd: string,
): boolean {
	return matchesFilesystemPattern(target, filesystem.denyWrite, cwd);
}

export function storageForm(target: string, cwd: string, isDirectory: boolean): string {
	const normalized = canonicalizePath(target, cwd).replace(/\/+$/g, "");
	const home = os.homedir();
	const display =
		normalized === home
			? "~"
			: normalized.startsWith(`${home}${path.sep}`)
				? `~${normalized.slice(home.length)}`
				: normalized;
	return isDirectory ? `${display}/` : display;
}

export function isGrantTooBroad(target: string, cwd: string): boolean {
	const normalized = canonicalizePath(target, cwd).replace(/\/+$/g, "");
	return normalized === "/" || normalized === os.homedir();
}
