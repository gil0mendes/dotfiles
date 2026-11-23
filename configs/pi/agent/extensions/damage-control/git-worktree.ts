import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import type { FilesystemGateConfig } from "./types";

/**
 * Git keeps a linked worktree's metadata outside its working directory: per-worktree state
 * (HEAD, index, FETCH_HEAD) lives in `<common>/worktrees/<name>`, shared state (objects, refs,
 * packed-refs, config) in the common dir. Without write access to both, git cannot fetch,
 * commit, or even refresh the index from inside the worktree.
 */
export type GitMetadataDirs = {
	gitDir: string;
	commonDir: string;
};

export type GitMetadataAccess = {
	allowWrite: string[];
	denyWrite: string[];
};

export const NO_GIT_METADATA_ACCESS: GitMetadataAccess = { allowWrite: [], denyWrite: [] };

const GIT_REV_PARSE_ARGS = ["rev-parse", "--path-format=absolute", "--git-dir", "--git-common-dir"];

/** Parses the two absolute paths printed by `git rev-parse --git-dir --git-common-dir`. */
export function parseGitMetadataDirs(output: string): GitMetadataDirs | null {
	const lines = output
		.split(/\r?\n/)
		.map((line) => line.trim())
		.filter((line) => line.length > 0);
	const [gitDir, commonDir] = lines;
	if (lines.length !== 2 || !gitDir || !commonDir) return null;
	if (!path.isAbsolute(gitDir) || !path.isAbsolute(commonDir)) return null;
	return { gitDir, commonDir };
}

/** Returns null when `cwd` is not inside a git repository or git is unavailable. */
export function resolveGitMetadataDirs(cwd: string): GitMetadataDirs | null {
	try {
		const output = execFileSync("git", GIT_REV_PARSE_ARGS, {
			cwd,
			encoding: "utf8",
			stdio: ["ignore", "pipe", "ignore"],
			timeout: 5_000,
		});
		return parseGitMetadataDirs(output);
	} catch {
		return null;
	}
}

function realPathOrSelf(target: string): string {
	try {
		return fs.realpathSync.native(target);
	} catch {
		return target;
	}
}

/**
 * Hooks run arbitrary code the next time anyone runs git in the main checkout, so they stay
 * read-only even though the rest of the git metadata becomes writable.
 */
export function gitMetadataAccessFor(dirs: GitMetadataDirs | null): GitMetadataAccess {
	if (!dirs) return NO_GIT_METADATA_ACCESS;
	const roots = [...new Set([realPathOrSelf(dirs.commonDir), realPathOrSelf(dirs.gitDir)])];
	return {
		allowWrite: roots,
		denyWrite: roots.map((root) => path.join(root, "hooks")),
	};
}

export function resolveGitMetadataAccess(cwd: string): GitMetadataAccess {
	return gitMetadataAccessFor(resolveGitMetadataDirs(cwd));
}

export function withGitMetadataAccess(
	config: FilesystemGateConfig,
	access: GitMetadataAccess,
): FilesystemGateConfig {
	return {
		...config,
		allowWrite: [...config.allowWrite, ...access.allowWrite],
		denyWrite: [...config.denyWrite, ...access.denyWrite],
	};
}
