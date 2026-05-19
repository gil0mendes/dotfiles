import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { DEFAULT_CONFIG_PATH } from "./config";
import { absoluteTarget, matchPathRules } from "./rules";
import type { Config, Rule } from "./types";

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

export function outsideCwdPath(target: string, cwd: string): string | null {
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

export function storageForm(target: string, isDirectory: boolean): string {
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

export function isGrantTooBroad(target: string): boolean {
	const normalized = target.replace(/\/+$|\\+$/g, "");
	return normalized === "/" || normalized === os.homedir();
}

function allowedOutsideRules(config: Config): Rule[] {
	return config.allowedOutsideCwdPaths.map((allowedPath) => ({
		path: allowedPath,
	}));
}

export function isOutsidePathAllowed(
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

export function persistAllowedOutsidePath(
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
