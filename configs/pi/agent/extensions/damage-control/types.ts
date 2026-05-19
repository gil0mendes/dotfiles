export type BashToolPatternConfig = {
	pattern: string;
	reason?: string;
	ask?: boolean;
};

export type PathPatternConfig = {
	path: string;
	reason?: string;
	ask?: boolean;
};

export type StrictModeAllowedCommandConfig = {
	pattern?: never;
	path?: never;
	command: string;
	reason?: string;
	ask?: never;
};

export type PatternConfig =
	| BashToolPatternConfig
	| PathPatternConfig
	| StrictModeAllowedCommandConfig;

export type BashDangerousPattern = BashToolPatternConfig & {
	description?: string;
	regex?: boolean;
};

export type FilesystemGateConfig = {
	denyRead: string[];
	allowRead?: string[];
	allowWrite: string[];
	denyWrite: string[];
	allowGitConfig?: boolean;
};

export type Config = {
	/** Enable command permission checks for bash tool calls. */
	permissionGate?: boolean;
	strictMode?: boolean;
	/** Enable structural built-in checks for dangerous shell commands. */
	useBuiltinCommandMatchers?: boolean;
	filesystem: FilesystemGateConfig;
	bashToolPatterns: BashToolPatternConfig[];
	strictModeAllowedList: StrictModeAllowedCommandConfig[];
};


