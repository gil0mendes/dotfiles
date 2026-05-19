export type Rule = {
	pattern?: string;
	path?: string;
	command?: string;
	reason?: string;
	ask?: boolean;
};

export type Config = {
	strictMode?: boolean;
	/** Enable structural built-in checks for dangerous shell commands. */
	useBuiltinCommandMatchers?: boolean;
	/** Ask before tools reference paths outside ctx.cwd. */
	askBeforeOutsideCwd?: boolean;
	allowedOutsideCwdPaths: string[];
	bashToolPatterns: Rule[];
	zeroAccessPaths: Rule[];
	readOnlyPaths: Rule[];
	noDeletePaths: Rule[];
	strictModeAllowedList: Rule[];
};
