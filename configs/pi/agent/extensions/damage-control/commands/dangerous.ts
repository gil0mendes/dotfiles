import { parseShell, walkCommands, wordToString } from "../shell/ast";

export type DangerousCommandMatch = {
	description: string;
	pattern: string;
};

type StructuralMatcher = (words: string[]) => string | undefined;

function hasArg(words: string[], prefix: string): boolean {
	return words.some((word) => word.startsWith(prefix));
}

function hasShortFlag(words: string[], flag: string): boolean {
	return words.some(
		(word) =>
			word === `-${flag}` ||
			(word.startsWith("-") && !word.startsWith("--") && word.includes(flag)),
	);
}

function hasLongOption(words: string[], option: string): boolean {
	return words.some((word) => word === `--${option}`);
}

function commandName(words: string[]): string | undefined {
	const first = words[0];
	if (!first) return undefined;
	return first.split("/").pop()?.toLowerCase();
}

const rmMatcher: StructuralMatcher = (words) => {
	if (commandName(words) !== "rm") return undefined;

	const hasRecursive =
		hasShortFlag(words, "r") ||
		hasShortFlag(words, "R") ||
		hasLongOption(words, "recursive") ||
		hasLongOption(words, "dir");
	const hasForce = hasShortFlag(words, "f") || hasLongOption(words, "force");

	return hasRecursive && hasForce ? "recursive force delete" : undefined;
};

const shredMatcher: StructuralMatcher = (words) =>
	commandName(words) === "shred" ? "secure file overwrite" : undefined;

const sudoMatcher: StructuralMatcher = (words) =>
	commandName(words) === "sudo" ? "superuser command" : undefined;

const doasMatcher: StructuralMatcher = (words) =>
	commandName(words) === "doas" ? "privileged command execution" : undefined;

const pkexecMatcher: StructuralMatcher = (words) =>
	commandName(words) === "pkexec" ? "privileged command execution" : undefined;

const ddMatcher: StructuralMatcher = (words) => {
	if (commandName(words) !== "dd") return undefined;
	return hasArg(words, "of=") ? "disk write operation" : undefined;
};

const mkfsMatcher: StructuralMatcher = (words) => {
	const name = commandName(words);
	return name === "mkfs" || name?.startsWith("mkfs.")
		? "filesystem format"
		: undefined;
};

const wipefsMatcher: StructuralMatcher = (words) =>
	commandName(words) === "wipefs" ? "filesystem signature wipe" : undefined;

const blkdiscardMatcher: StructuralMatcher = (words) =>
	commandName(words) === "blkdiscard" ? "block device discard" : undefined;

const partitionMatcher: StructuralMatcher = (words) => {
	const name = commandName(words);
	return name === "fdisk" ||
		name === "sfdisk" ||
		name === "cfdisk" ||
		name === "parted" ||
		name === "sgdisk"
		? "disk partitioning"
		: undefined;
};

const chmodMatcher: StructuralMatcher = (words) => {
	if (commandName(words) !== "chmod") return undefined;

	const hasRecursive = hasShortFlag(words, "R") || hasLongOption(words, "recursive");
	const hasWorldWritable = words.some(
		(word) =>
			word === "777" ||
			word === "0777" ||
			word === "7777" ||
			word === "1777" ||
			word === "a+rwx" ||
			word === "ugo+rwx",
	);

	return hasRecursive && hasWorldWritable
		? "insecure recursive permissions"
		: undefined;
};

const chownMatcher: StructuralMatcher = (words) => {
	if (commandName(words) !== "chown") return undefined;
	const hasRecursive = hasShortFlag(words, "R") || hasLongOption(words, "recursive");
	return hasRecursive ? "recursive ownership change" : undefined;
};

const containerMatcher: StructuralMatcher = (words) => {
	const name = commandName(words);
	if (name !== "docker" && name !== "podman") return undefined;
	if (words[1] !== "run" && words[1] !== "create") return undefined;

	const hasPrivileged = words.some(
		(word) => word === "--privileged" || word.startsWith("--privileged="),
	);
	const hasHostPid = words.some((word) => word === "--pid=host");
	const hasHostNetwork = words.some((word) => word === "--network=host" || word === "--net=host");
	const hasHostUsers = words.some((word) => word === "--userns=host");
	const hasHostUts = words.some((word) => word === "--uts=host");
	const hasHostIpc = words.some((word) => word === "--ipc=host");
	const hasRootMount = words.some(
		(word) =>
			word.startsWith("-v/:") ||
			word.startsWith("--volume=/:") ||
			word.startsWith("--mount=type=bind,source=/,"),
	);
	const hasContainerSocket = words.some(
		(word) =>
			word.includes("/var/run/docker.sock") ||
			word.includes("/run/docker.sock") ||
			word.includes("/var/run/podman.sock") ||
			word.includes("/run/podman.sock"),
	);

	if (hasPrivileged) return "container with privileged mode";
	if (hasHostPid) return "container with host PID namespace";
	if (hasHostNetwork) return "container with host network";
	if (hasHostUsers) return "container with host user namespace";
	if (hasHostUts) return "container with host UTS namespace";
	if (hasHostIpc) return "container with host IPC";
	if (hasRootMount) return "container with root filesystem mount";
	if (hasContainerSocket) return "container with container socket access";
	return undefined;
};

const builtinMatchers: StructuralMatcher[] = [
	rmMatcher,
	shredMatcher,
	sudoMatcher,
	doasMatcher,
	pkexecMatcher,
	ddMatcher,
	mkfsMatcher,
	wipefsMatcher,
	blkdiscardMatcher,
	partitionMatcher,
	chmodMatcher,
	chownMatcher,
	containerMatcher,
];

export const BUILTIN_DANGEROUS_COMMAND_COUNT = builtinMatchers.length;

function matchWords(words: string[]): DangerousCommandMatch | null {
	if (words.length === 0) return null;
	for (const matcher of builtinMatchers) {
		const description = matcher(words);
		if (description) return { description, pattern: words[0] ?? "unknown" };
	}
	return null;
}

export function matchDangerousCommand(command: string): DangerousCommandMatch | null {
	const parsed = parseShell(command);
	if (!parsed) return null;

	let match: DangerousCommandMatch | null = null;
	walkCommands(parsed, (cmd) => {
		const words = (cmd.words ?? []).map(wordToString);
		match = matchWords(words);
		return match !== null;
	});
	return match;
}
