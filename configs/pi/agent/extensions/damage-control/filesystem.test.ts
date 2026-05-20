import * as path from "node:path";
import { describe, expect, it } from "vitest";
import {
	isWriteDenied,
	matchesFilesystemPattern,
	shouldPromptForRead,
	shouldPromptForWrite,
} from "./filesystem";
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
});
