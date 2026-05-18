import { homedir } from "node:os";
import {
	Container,
	Key,
	matchesKey,
	Spacer,
	Text,
	visibleWidth,
} from "@earendil-works/pi-tui";

export type PromptResult =
	| "allow-file-once"
	| "allow-dir-once"
	| "allow-file-session"
	| "allow-dir-session"
	| "allow-file-always"
	| "allow-dir-always"
	| "deny";

type PromptOption = {
	label: string;
	result: PromptResult;
};

const FILE_OPTIONS: PromptOption[] = [
	{ label: "Allow once", result: "allow-file-once" },
	{ label: "Allow file this session", result: "allow-file-session" },
	{ label: "Allow file always", result: "allow-file-always" },
	{ label: "Allow directory this session", result: "allow-dir-session" },
	{ label: "Allow directory always", result: "allow-dir-always" },
	{ label: "Deny", result: "deny" },
];

const DIR_OPTIONS: PromptOption[] = [
	{ label: "Allow once", result: "allow-dir-once" },
	{ label: "Allow directory this session", result: "allow-dir-session" },
	{ label: "Allow directory always", result: "allow-dir-always" },
	{ label: "Deny", result: "deny" },
];

function displayCwd(cwd: string): string {
	const home = homedir();
	if (cwd === home) return "~";
	if (cwd.startsWith(`${home}/`) || cwd.startsWith(`${home}\\`))
		return `~${cwd.slice(home.length)}`;
	return cwd;
}

export function createPathAccessPromptComponent(
	toolName: string,
	displayPath: string,
	displayDir: string,
	cwd: string,
	showFileOptions: boolean,
) {
	return (
		tui: { requestRender(): void },
		theme: {
			fg(color: string, text: string): string;
			bg(color: string, text: string): string;
			bold(text: string): string;
		},
		_kb: unknown,
		done: (result: PromptResult) => void,
	) => {
		const options = showFileOptions ? FILE_OPTIONS : DIR_OPTIONS;
		let selectedIndex = 0;

		const container = new Container();
		const border = (s: string) => theme.fg("warning", s);

		container.addChild(
			new Text(
				theme.fg("warning", theme.bold("Outside Workspace Access")),
				1,
				0,
			),
		);
		container.addChild(new Spacer(1));
		container.addChild(
			new Text(
				theme.fg(
					"text",
					`\`${toolName}\` targets a path outside the working directory.`,
				),
				1,
				0,
			),
		);
		container.addChild(new Spacer(1));
		container.addChild(
			new Text(theme.fg("dim", `  Cwd:  ${displayCwd(cwd)}`), 1, 0),
		);
		container.addChild(
			new Text(theme.fg("dim", `  Path: ${displayPath}`), 1, 0),
		);
		container.addChild(
			new Text(theme.fg("dim", `  Dir:  ${displayDir}`), 1, 0),
		);
		container.addChild(new Spacer(1));

		const optionLines = options.map(() => new Text("", 1, 0));
		for (const line of optionLines) container.addChild(line);

		container.addChild(new Spacer(1));
		container.addChild(
			new Text(
				theme.fg("dim", "↑/↓/Tab select · Enter select · Esc deny"),
				1,
				0,
			),
		);

		const renderOptions = () => {
			options.forEach((option, index) => {
				const label = ` ${option.label} `;
				optionLines[index]?.setText(
					index === selectedIndex
						? theme.bg("selectedBg", theme.fg("accent", label))
						: theme.fg("dim", label),
				);
			});
		};

		const moveSelection = (direction: number) => {
			selectedIndex =
				(selectedIndex + direction + options.length) % options.length;
			renderOptions();
			tui.requestRender();
		};

		renderOptions();

		return {
			render: (width: number) => {
				const innerWidth = Math.max(1, width - 2);
				const contentWidth = Math.max(1, width - 4);
				const raw = container.render(contentWidth);
				const top = border(`╭${"─".repeat(innerWidth)}╮`);
				const bottom = border(`╰${"─".repeat(innerWidth)}╯`);
				const left = border("│");
				const right = border("│");
				return [
					top,
					...raw.map(
						(line) =>
							`${left} ${line}${" ".repeat(Math.max(0, contentWidth - visibleWidth(line)))} ${right}`,
					),
					bottom,
				];
			},
			invalidate: () => container.invalidate(),
			handleInput: (data: string) => {
				if (
					matchesKey(data, Key.up) ||
					data === "k" ||
					matchesKey(data, Key.shift("tab"))
				)
					return moveSelection(-1);
				if (
					matchesKey(data, Key.down) ||
					data === "j" ||
					matchesKey(data, Key.tab)
				)
					return moveSelection(1);
				if (matchesKey(data, Key.enter))
					return done(options[selectedIndex]?.result ?? "deny");
				if (matchesKey(data, Key.escape)) done("deny");
			},
		};
	};
}
