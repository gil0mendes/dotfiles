import * as path from "node:path";
import { describe, expect, it } from "vitest";
import {
	isWriteDenied,
	matchesFilesystemPattern,
	shouldPromptForRead,
	shouldPromptForWrite,
} from "./filesystem";
import { extractSandboxBlockedAccesses } from "./index";
import type { FilesystemGateConfig } from "./types";

const cwd = path.resolve("/tmp/example-project");

const filesystem: FilesystemGateConfig = {
	denyRead: ["/", "*.env", "~/.ssh/"],
	allowRead: [".", "/tmp/shared/"],
	allowWrite: [".", "/tmp"],
	denyWrite: [".env", "*.key"],
};

describe("filesystem gate", () => {
	it("lets allowRead override broad denyRead", () => {
		expect(shouldPromptForRead(path.join(cwd, "README.md"), filesystem, cwd)).toBe(false);
	});

	it("prompts for denied reads outside allowRead", () => {
		expect(shouldPromptForRead("/private/secret.txt", filesystem, cwd)).toBe(true);
	});

	it("requires writes to match allowWrite", () => {
		expect(shouldPromptForWrite(path.join(cwd, "file.txt"), filesystem, cwd)).toBe(false);
		expect(shouldPromptForWrite("/var/log/file.txt", filesystem, cwd)).toBe(true);
	});

	it("matches denyWrite patterns", () => {
		expect(isWriteDenied(path.join(cwd, ".env"), filesystem, cwd)).toBe(true);
		expect(isWriteDenied(path.join(cwd, "private.key"), filesystem, cwd)).toBe(true);
	});

	it("matches directory patterns as prefixes", () => {
		expect(matchesFilesystemPattern("/tmp/shared/note.txt", ["/tmp/shared/"], cwd)).toBe(true);
	});

	it("extracts read blocks reported by sandboxed commands", () => {
		expect(
			extractSandboxBlockedAccesses("cat: /private/secret.txt: Operation not permitted", [], cwd),
		).toEqual([{ kind: "read", path: "/private/secret.txt" }]);
	});

	it("extracts write blocks reported by sandboxed shell redirects", () => {
		expect(
			extractSandboxBlockedAccesses("/bin/bash: /opt/file.txt: Operation not permitted", [], cwd),
		).toEqual([{ kind: "write", path: "/opt/file.txt" }]);
	});

	it("extracts read and write blocks from macOS sandbox violations", () => {
		expect(
			extractSandboxBlockedAccesses(
				"",
				[
					"bash(123) deny(1) file-read-data /private/secret.txt",
					"bash(123) deny(1) file-write-create /opt/file.txt",
				],
				cwd,
			),
		).toEqual([
			{ kind: "read", path: "/private/secret.txt" },
			{ kind: "write", path: "/opt/file.txt" },
		]);
	});
});
