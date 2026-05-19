import {
	parse,
	type Command,
	type Program,
	type SimpleCommand,
	type Statement,
	type Word,
	type WordPart,
} from "@aliou/sh";

/**
 * Strips heredoc bodies from a shell command string so that pattern matchers
 * and path extractors only see the outer command, not its stdin data.
 */
export function stripHeredocs(command: string): string {
	const openerRe = /<<(-?)\s*(['"]?)([A-Za-z0-9_]+)\2/g;
	let result = command;
	let match: RegExpExecArray | null;
	const replacements: Array<{ start: number; end: number }> = [];

	while ((match = openerRe.exec(command)) !== null) {
		const marker = match[3];
		const bodyStart = command.indexOf("\n", match.index);
		if (bodyStart === -1) continue;
		const closingRe = new RegExp(`^\\t*${marker}\\s*$`, "m");
		const bodySlice = command.slice(bodyStart + 1);
		const closeMatch = closingRe.exec(bodySlice);
		if (!closeMatch) continue;
		const bodyEnd =
			bodyStart + 1 + closeMatch.index + closeMatch[0].length;
		replacements.push({ start: bodyStart + 1, end: bodyEnd });
	}

	for (let i = replacements.length - 1; i >= 0; i--) {
		const r = replacements[i];
		if (!r) continue;
		result = result.slice(0, r.start) + " " + result.slice(r.end);
	}

	return result;
}

export function parseShell(command: string): Program | null {
	try {
		return parse(command, { dialect: "bash" }).ast;
	} catch {
		const stripped = stripHeredocs(command);
		if (stripped === command) return null;
		try {
			return parse(stripped, { dialect: "bash" }).ast;
		} catch {
			return null;
		}
	}
}

export function wordToString(word: Word): string {
	return word.parts.map(wordPartToString).join("");
}

function wordPartToString(part: WordPart): string {
	switch (part.type) {
		case "Literal":
		case "SglQuoted":
			return part.value;
		case "DblQuoted":
			return part.parts.map(wordPartToString).join("");
		case "ParamExp":
			return part.short
				? `$${part.param.value}`
				: `\${${part.param.value}${part.op ?? ""}${part.value ? wordToString(part.value) : ""}}`;
		case "CmdSubst":
			return "$(...)";
		case "ArithExp":
			return `$((${part.expr}))`;
		case "ProcSubst":
			return `${part.op}(...)`;
	}
}

export function walkCommands(
	node: Program,
	callback: (cmd: SimpleCommand) => boolean | undefined,
): void {
	for (const stmt of node.body) {
		if (walkStatement(stmt, callback)) return;
	}
}

function walkStatement(
	stmt: Statement,
	callback: (cmd: SimpleCommand) => boolean | undefined,
): boolean {
	return walkCommand(stmt.command, callback);
}

export function walkStatements(
	stmts: Statement[],
	callback: (cmd: SimpleCommand) => boolean | undefined,
): boolean {
	for (const stmt of stmts) {
		if (walkStatement(stmt, callback)) return true;
	}
	return false;
}

function walkCommand(
	cmd: Command,
	callback: (cmd: SimpleCommand) => boolean | undefined,
): boolean {
	switch (cmd.type) {
		case "SimpleCommand":
			return callback(cmd) === true;
		case "Pipeline":
			return walkStatements(cmd.commands, callback);
		case "Logical":
			return (
				walkStatement(cmd.left, callback) || walkStatement(cmd.right, callback)
			);
		case "Subshell":
		case "Block":
			return walkStatements(cmd.body, callback);
		case "IfClause":
			return (
				walkStatements(cmd.cond, callback) ||
				walkStatements(cmd.then, callback) ||
				(cmd.else ? walkStatements(cmd.else, callback) : false)
			);
		case "ForClause":
		case "SelectClause":
			return walkStatements(cmd.body, callback);
		case "WhileClause":
			return (
				walkStatements(cmd.cond, callback) || walkStatements(cmd.body, callback)
			);
		case "CaseClause":
			for (const item of cmd.items) {
				if (walkStatements(item.body, callback)) return true;
			}
			return false;
		case "FunctionDecl":
			return walkStatements(cmd.body, callback);
		case "TimeClause":
			return walkStatement(cmd.command, callback);
		case "CoprocClause":
			return walkStatement(cmd.body, callback);
		case "CStyleLoop":
			return walkStatements(cmd.body, callback);
		case "TestClause":
		case "ArithCmd":
		case "DeclClause":
		case "LetClause":
			return false;
	}
}
