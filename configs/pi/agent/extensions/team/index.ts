import type {
	ExtensionAPI,
	ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { Runtime, team } from "./runtime";

type ProcessHooks = {
	once(event: "SIGINT", listener: () => void): unknown;
	on(event: "beforeExit", listener: () => void): unknown;
	exit(code?: number): never;
};

// Process-level cleanup for subagents on exit
const processHooksSetupKey = Symbol.for("team.processHooksSetup");
const globalWithProcessHooks = globalThis as typeof globalThis &
	Record<symbol, boolean | undefined>;

function setupProcessHooks(
	runtime: Runtime,
	processHooks: ProcessHooks,
	setupKey: symbol,
) {
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

function registerPiHooks(pi: ExtensionAPI) {
	let currentCtx: ExtensionContext | undefined;

	const refreshWidget = () => {
		// TODO: implement
	};

	const activateSession = (ctx: ExtensionContext) => {
		currentCtx = ctx;
		team.activateSession(
			{
				sessionId: ctx.sessionManager.getSessionId(),
				sendMessage: pi.sendMessage.bind(pi),
				isIdle: () => ctx.isIdle(),
			},
			refreshWidget,
		);
	};

	pi.on("session_start", (_event, ctx) => {
		activateSession(ctx);
	});

	pi.on("session_shutdown", (event, ctx) => {
		const sessionId = ctx.sessionManager.getSessionId();
		team.terminateSession(sessionId);

		if (event.reason === "quit") {
			team.abortAll();
		}
	});
}

export default function teamExtension(pi: ExtensionAPI) {
	setupProcessHooks(team, process, processHooksSetupKey);
	registerPiHooks(pi);

	// TODO: register tools
	// TODO: register renderers
}
