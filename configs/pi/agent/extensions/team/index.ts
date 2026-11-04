import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Runtime, team } from "./runtime";
import { setupTools } from "./tools/registerTools";
import { registerUIRenderers } from "./ui";
import { createFleetController } from "./ui/controller";
import { registerSystemPrompt } from "./systemPrompt";
import { registerTeamCommand } from "./commands";

type ProcessHooks = {
	once(event: "SIGINT", listener: () => void): unknown;
	on(event: "beforeExit", listener: () => void): unknown;
	exit(code?: number): never;
};

// Process-level cleanup for subagents on exit
const processHooksSetupKey = Symbol.for("team.processHooksSetup");
const globalWithProcessHooks = globalThis as typeof globalThis &
	Record<symbol, boolean | undefined>;

function setupProcessHooks(runtime: Runtime, processHooks: ProcessHooks, setupKey: symbol) {
	if (globalWithProcessHooks[setupKey]) {
		return;
	}

	processHooks.once("SIGINT", () => {
		team.abortAll();
		processHooks.exit(130);
	});

	processHooks.on("beforeExit", () => team.abortAll());

	globalWithProcessHooks[setupKey] = true;
}

function registerPiHooks(pi: ExtensionAPI, disposeFleetOverlay: () => void) {
	const activateSession = (ctx: ExtensionContext) => {
		team.activateSession({
			cwd: ctx.cwd,
			sessionId: ctx.sessionManager.getSessionId(),
			sendMessage: pi.sendMessage.bind(pi),
			isIdle: () => ctx.isIdle(),
			events: pi.events,
		});
	};

	pi.on("session_start", (_event, ctx) => {
		disposeFleetOverlay();
		activateSession(ctx);
	});

	pi.on("session_shutdown", (event, ctx) => {
		disposeFleetOverlay();
		const sessionId = ctx.sessionManager.getSessionId();
		team.terminateSession(sessionId);

		if (event.reason === "quit") {
			team.abortAll();
		}
	});
}

export default function teamExtension(pi: ExtensionAPI) {
	setupProcessHooks(team, process, processHooksSetupKey);
	const fleetController = createFleetController(pi.events, team);
	registerUIRenderers(pi, team);
	registerPiHooks(pi, fleetController.dispose);
	setupTools(pi, team);
	registerTeamCommand(pi, team, fleetController.show);
	registerSystemPrompt(pi, team);
}
