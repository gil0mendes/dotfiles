import { realpathSync } from "node:fs";
import { dirname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import {
	DefaultPackageManager,
	SettingsManager,
	getAgentDir,
	type ExtensionAPI,
	type ExtensionContext,
	type ResolvedPaths,
	type ResolvedResource,
} from "@earendil-works/pi-coding-agent";
import { TOGGLE_SHORTCUT, TogglePicker, type ToggleChange, type ToggleOption } from "./picker.js";

const resourceTypes = ["extensions", "skills", "prompts", "themes"] as const;
type ResourceType = (typeof resourceTypes)[number];

interface ResourceOption extends ToggleOption {
	type: ResourceType;
	scope: "user" | "project";
	resource: ResolvedResource;
	settingsDir: string;
}

function canonical(path: string): string {
	try {
		return realpathSync(path);
	} catch {
		return resolve(path);
	}
}

export function toggleFilter(
	paths: string[],
	path: string,
	enabled: boolean,
	baseDirs: readonly string[] = [],
	skill = false,
): string[] {
	const targets = skill ? [path, dirname(path)] : [path];
	return [
		...paths.filter((entry) => {
			if (!entry.startsWith("+") && !entry.startsWith("-")) return true;
			const normalize = (value: string) => value.replaceAll("\\", "/").replace(/^\.\//, "");
			const value = normalize(entry.slice(1));
			return !targets.some(
				(target) =>
					value === normalize(target) ||
					baseDirs.some((base) => normalize(relative(base, target)) === value),
			);
		}),
		`${enabled ? "+" : "-"}${path}`,
	];
}

function buildOptions(resources: ResolvedPaths, agentDir: string, cwd: string): ResourceOption[] {
	const self = canonical(dirname(fileURLToPath(import.meta.url)));
	return resourceTypes.flatMap((type) =>
		resources[type].flatMap((resource): ResourceOption[] => {
			const { scope, origin } = resource.metadata;
			if (scope === "temporary" || origin !== "top-level" || resource.path.startsWith("builtin:"))
				return [];
			if (type === "extensions") {
				const path = canonical(resource.path);
				if (path === self || path.startsWith(`${self}${sep}`)) return [];
			}
			const settingsDir = scope === "project" ? join(cwd, ".pi") : agentDir;
			const displayPath = relative(resource.metadata.baseDir ?? settingsDir, resource.path);
			return [
				{
					type,
					scope,
					resource,
					settingsDir,
					enabled: resource.enabled,
					label: `${scope === "project" ? "Project" : "Global"} · ${type} · ${displayPath}`,
				},
			];
		}),
	);
}

function applyChanges(settings: SettingsManager, changes: ToggleChange<ResourceOption>[]) {
	const setters = {
		user: {
			extensions: settings.setExtensionPaths,
			skills: settings.setSkillPaths,
			prompts: settings.setPromptTemplatePaths,
			themes: settings.setThemePaths,
		},
		project: {
			extensions: settings.setProjectExtensionPaths,
			skills: settings.setProjectSkillPaths,
			prompts: settings.setProjectPromptTemplatePaths,
			themes: settings.setProjectThemePaths,
		},
	};
	for (const { option, enabled } of changes) {
		const { type, scope, resource, settingsDir } = option;
		const current =
			scope === "project" ? settings.getProjectSettings() : settings.getGlobalSettings();
		const paths = toggleFilter(
			current[type] ?? [],
			resource.path,
			enabled,
			[resource.metadata.baseDir ?? settingsDir],
			type === "skills" && resource.path.endsWith(`${sep}SKILL.md`),
		);
		setters[scope][type].call(settings, paths);
	}
}

function assertSettingsOK(settings: SettingsManager) {
	const errors = settings.drainErrors();
	if (errors.length)
		throw new Error(errors.map(({ scope, error }) => `${scope}: ${error.message}`).join("; "));
}

export default function (pi: Pick<ExtensionAPI, "registerCommand" | "registerShortcut">) {
	let running = false;

	async function run(ctx: ExtensionContext, reload?: () => Promise<void>) {
		if (ctx.mode !== "tui") {
			ctx.ui.notify("Extension toggle requires the terminal UI", "warning");
			return;
		}
		if (running) return;
		running = true;
		try {
			const agentDir = getAgentDir();
			const createSettings = () =>
				SettingsManager.create(ctx.cwd, agentDir, { projectTrusted: ctx.isProjectTrusted() });
			const settingsManager = createSettings();
			assertSettingsOK(settingsManager);
			const manager = new DefaultPackageManager({ cwd: ctx.cwd, agentDir, settingsManager });
			const options = buildOptions(await manager.resolve(async () => "skip"), agentDir, ctx.cwd);
			if (!options.length) {
				ctx.ui.notify("No toggleable local Pi resources found", "info");
				return;
			}
			const changes = await ctx.ui.custom<ToggleChange<ResourceOption>[] | null>(
				(tui, theme, _keybindings, done) =>
					new TogglePicker(
						options,
						theme,
						() => Math.min(12, Math.floor(tui.terminal.rows * 0.8) - 3),
						() => tui.requestRender(),
						done,
					),
				{
					overlay: true,
					overlayOptions: { anchor: "center", width: "85%", maxHeight: "80%" },
				},
			);
			if (!changes?.length) return;

			const fresh = createSettings();
			assertSettingsOK(fresh);
			if (changes.some(({ option }) => option.scope === "project") && !fresh.isProjectTrusted()) {
				throw new Error("Project is no longer trusted");
			}
			applyChanges(fresh, changes);
			await fresh.flush();
			assertSettingsOK(fresh);
			const summary = `Saved ${changes.length} resource ${changes.length === 1 ? "change" : "changes"}.`;
			if (reload && (await ctx.ui.confirm("Reload Pi?", `${summary} Reload now to apply?`))) {
				await reload();
				return;
			}
			ctx.ui.notify(`${summary} Run /reload to apply.`, "info");
		} catch (error) {
			ctx.ui.notify(
				`Extension toggle failed: ${error instanceof Error ? error.message : String(error)}`,
				"error",
			);
		} finally {
			running = false;
		}
	}

	pi.registerCommand("extension-toggle", {
		description: "Search and toggle local Pi extensions, skills, prompts, and themes",
		handler: async (_args, ctx) => {
			await ctx.waitForIdle();
			await run(ctx, () => ctx.reload());
		},
	});
	pi.registerShortcut(TOGGLE_SHORTCUT, {
		description: "Open the Pi resource picker",
		handler: async (ctx) => {
			if (ctx.isIdle()) await run(ctx);
		},
	});
}
