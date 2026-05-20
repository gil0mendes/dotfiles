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

export type Action =
	| {
			kind: "file";
			path: string;
			origin?: string;
	  }
	| {
			kind: "command";
			command: string;
			origin?: string;
	  };

export type RuleResult<TMeta = null> =
	| {
			kind: "pass";
	  }
	| {
			kind: "match";
			reason: string;
			metadata: TMeta;
	  };

export type Rule<TMeta = null> = {
	key: string;
	check: (action: Action) => RuleResult<TMeta>;
};

export type Safety<TMeta = null> =
	| {
			kind: "safe";
	  }
	| {
			kind: "dangerous";
			action: Action;
			key: string;
			reason: string;
			metadata: TMeta;
	  };
