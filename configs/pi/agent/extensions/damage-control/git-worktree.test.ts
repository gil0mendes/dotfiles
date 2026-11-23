// Run from configs/pi: bunx --no-install vitest run agent/extensions/damage-control/git-worktree.test.ts
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { isWriteDenied, shouldPromptForWrite } from "./filesystem";
import {
	gitMetadataAccessFor,
	NO_GIT_METADATA_ACCESS,
	parseGitMetadataDirs,
	resolveGitMetadataAccess,
	resolveGitMetadataDirs,
	withGitMetadataAccess,
} from "./git-worktree";
import type { FilesystemGateConfig } from "./types";

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

describe("parseGitMetadataDirs", () => {
	it("reads git-dir and common-dir from rev-parse output", () => {
		expect(parseGitMetadataDirs("/repo/.git/worktrees/wt\n/repo/.git\n")).toEqual({
			gitDir: "/repo/.git/worktrees/wt",
			commonDir: "/repo/.git",
		});
	});

	it("tolerates CRLF line endings", () => {
		expect(parseGitMetadataDirs("/repo/.git\r\n/repo/.git\r\n")).toEqual({
			gitDir: "/repo/.git",
			commonDir: "/repo/.git",
		});
	});

	it.each([
		["empty output", ""],
		["a single path", "/repo/.git\n"],
		["extra lines", "/repo/.git\n/repo/.git\n/repo/.git\n"],
		["relative paths", ".git\n.git\n"],
	])("rejects %s", (_name, output) => {
		expect(parseGitMetadataDirs(output)).toBeNull();
	});
});

describe("gitMetadataAccessFor", () => {
	it("grants nothing outside a git repository", () => {
		expect(gitMetadataAccessFor(null)).toBe(NO_GIT_METADATA_ACCESS);
	});

	it("allows the worktree git-dir and common dir but keeps hooks read-only", () => {
		const access = gitMetadataAccessFor({
			gitDir: "/nonexistent/repo/.git/worktrees/wt",
			commonDir: "/nonexistent/repo/.git",
		});
		expect(access.allowWrite).toEqual([
			"/nonexistent/repo/.git",
			"/nonexistent/repo/.git/worktrees/wt",
		]);
		expect(access.denyWrite).toEqual([
			"/nonexistent/repo/.git/hooks",
			"/nonexistent/repo/.git/worktrees/wt/hooks",
		]);
	});

	it("does not repeat a git-dir that is its own common dir", () => {
		const access = gitMetadataAccessFor({
			gitDir: "/nonexistent/repo/.git",
			commonDir: "/nonexistent/repo/.git",
		});
		expect(access.allowWrite).toEqual(["/nonexistent/repo/.git"]);
		expect(access.denyWrite).toEqual(["/nonexistent/repo/.git/hooks"]);
	});
});

describe("withGitMetadataAccess", () => {
	it("appends to the configured lists without mutating them", () => {
		const base: FilesystemGateConfig = {
			denyRead: ["/secret"],
			allowRead: ["."],
			allowWrite: ["."],
			denyWrite: [".env"],
			allowGitConfig: true,
		};
		const merged = withGitMetadataAccess(base, { allowWrite: ["/g"], denyWrite: ["/g/hooks"] });
		expect(merged).toEqual({
			...base,
			allowWrite: [".", "/g"],
			denyWrite: [".env", "/g/hooks"],
		});
		expect(base.allowWrite).toEqual(["."]);
		expect(base.denyWrite).toEqual([".env"]);
	});
});

describe("linked worktree on disk", () => {
	let sandboxRoot: string;
	let mainCheckout: string;
	let worktree: string;
	let plainRepo: string;
	let notARepo: string;

	beforeAll(() => {
		sandboxRoot = fs.realpathSync.native(
			fs.mkdtempSync(path.join(os.tmpdir(), "dc-git-worktree-")),
		);
		mainCheckout = path.join(sandboxRoot, "main");
		worktree = path.join(sandboxRoot, "worktrees", "feature");
		plainRepo = path.join(sandboxRoot, "plain");
		notARepo = path.join(sandboxRoot, "not-a-repo");
		for (const dir of [mainCheckout, plainRepo, notARepo]) fs.mkdirSync(dir, { recursive: true });
		git(mainCheckout, "init", "-q", "-b", "main");
		git(mainCheckout, "commit", "-q", "--allow-empty", "-m", "init");
		git(mainCheckout, "worktree", "add", "-q", "-b", "feature", worktree);
		git(plainRepo, "init", "-q", "-b", "main");
	});

	afterAll(() => {
		fs.rmSync(sandboxRoot, { recursive: true, force: true });
	});

	it("resolves the git-dir and common dir that live outside the worktree", () => {
		expect(resolveGitMetadataDirs(worktree)).toEqual({
			gitDir: path.join(mainCheckout, ".git", "worktrees", "feature"),
			commonDir: path.join(mainCheckout, ".git"),
		});
	});

	it("resolves the same metadata from a subdirectory of the worktree", () => {
		const subdirectory = path.join(worktree, "nested");
		fs.mkdirSync(subdirectory);
		expect(resolveGitMetadataDirs(subdirectory)?.commonDir).toBe(path.join(mainCheckout, ".git"));
	});

	it("returns null outside a git repository", () => {
		expect(resolveGitMetadataDirs(notARepo)).toBeNull();
		expect(resolveGitMetadataAccess(notARepo)).toBe(NO_GIT_METADATA_ACCESS);
	});

	it("is a no-op for a plain checkout, whose .git already sits inside cwd", () => {
		const access = resolveGitMetadataAccess(plainRepo);
		expect(access.allowWrite).toEqual([path.join(plainRepo, ".git")]);
	});

	describe("filesystem gate with the resolved access", () => {
		const gate = (): FilesystemGateConfig =>
			withGitMetadataAccess(
				{ denyRead: [], allowWrite: ["."], denyWrite: [".env"] },
				resolveGitMetadataAccess(worktree),
			);

		it("lets git write the per-worktree and shared metadata that fetch needs", () => {
			const filesystem = gate();
			const writes = [
				path.join(mainCheckout, ".git", "worktrees", "feature", "FETCH_HEAD"),
				path.join(mainCheckout, ".git", "worktrees", "feature", "index.lock"),
				path.join(mainCheckout, ".git", "refs", "remotes", "origin", "main"),
				path.join(mainCheckout, ".git", "packed-refs.lock"),
				path.join(mainCheckout, ".git", "objects", "pack", "tmp_pack"),
			];
			for (const target of writes) {
				expect(shouldPromptForWrite(target, filesystem, worktree), target).toBe(false);
				expect(isWriteDenied(target, filesystem, worktree), target).toBe(false);
			}
		});

		it("keeps hooks read-only", () => {
			const filesystem = gate();
			expect(
				isWriteDenied(path.join(mainCheckout, ".git", "hooks", "pre-commit"), filesystem, worktree),
			).toBe(true);
		});

		it("does not open the main checkout's working tree", () => {
			const filesystem = gate();
			expect(shouldPromptForWrite(path.join(mainCheckout, "README.md"), filesystem, worktree)).toBe(
				true,
			);
			expect(
				shouldPromptForWrite(
					path.join(sandboxRoot, "worktrees", "other", "file"),
					filesystem,
					worktree,
				),
			).toBe(true);
		});

		it("is closed without the git metadata access", () => {
			const closed: FilesystemGateConfig = { denyRead: [], allowWrite: ["."], denyWrite: [".env"] };
			const fetchHead = path.join(mainCheckout, ".git", "worktrees", "feature", "FETCH_HEAD");
			expect(shouldPromptForWrite(fetchHead, closed, worktree)).toBe(true);
		});
	});
});
