import { spawn } from "node:child_process";
import { Plugin } from "@opencode/plugin";

function inputString(input: unknown, key: string): string | undefined {
	if (typeof input !== "object" || input === null) return undefined;
	const value = Object.fromEntries(Object.entries(input))[key];
	return typeof value === "string" ? value : undefined;
}

function inputBoolean(input: unknown, key: string): boolean | undefined {
	if (typeof input !== "object" || input === null) return undefined;
	const value = Object.fromEntries(Object.entries(input))[key];
	return typeof value === "boolean" ? value : undefined;
}

function executeAstGrep(args: string[]): Promise<string> {
	return new Promise((resolve, reject) => {
		const process = spawn("ast-grep", args);
		let stdout = "";
		let stderr = "";
		process.stdout.on("data", (chunk: Buffer) => { stdout += chunk.toString(); });
		process.stderr.on("data", (chunk: Buffer) => { stderr += chunk.toString(); });
		process.once("error", reject);
		process.once("exit", (code) => {
			if (code === 0 || !stderr.trim()) resolve(stdout || "No matches found.");
			else resolve(`Error: ${stderr}`);
		});
	});
}

export default Plugin.define({
	id: "ast-grep",
	async setup(ctx) {
		await ctx.tool.transform((editor) => {
			editor.add({
				name: "ast_grep_search",
				description: "Search code with ast-grep structural patterns.",
				input: {
					type: "object",
					properties: { pattern: { type: "string" }, path: { type: "string" }, lang: { type: "string" }, json: { type: "boolean" } },
					required: ["pattern"],
					additionalProperties: false,
				},
				async execute(input) {
					const pattern = inputString(input, "pattern");
					if (!pattern) return { content: "Error: pattern is required." };
					const args = ["--pattern", pattern];
					const lang = inputString(input, "lang");
					if (lang) args.push("--lang", lang);
					if (inputBoolean(input, "json")) args.push("--json");
					args.push(inputString(input, "path") ?? ".");
					return { content: await executeAstGrep(args) };
				},
			});
		});
	},
});
