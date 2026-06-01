import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { SubagentSessionRunner, type SubagentRunner } from "./subagentRunner";

export type ActiveRuntimeBinding = {
	sessionId: string;
	sendMessage: ExtensionAPI["sendMessage"];
	isIdle: () => boolean;
};

export type SubAgentState = {
	id: string;
};

export class Runtime {
	private readonly agents = new Map<string, SubAgentState>();
	private readonly runner: SubagentRunner;
	private readonly scheduleFlush: (callback: () => void) => void;
	private readonly now = Date.now;

	constructor() {
		// TODO: move this logic outside to make it more modular
		this.scheduleFlush = (fn) => setTimeout(fn, 0);
		this.runner = new SubagentSessionRunner();
	}

	abortAll() {
		// TODO: terminate all running agents
	}

	activateSession(binding: ActiveRuntimeBinding, refreshHandler?: () => void) {
		// TODO:
	}

	terminateSession(sessionId: string): void {
		// TODO
	}
}

const runtimeKey = Symbol.for("team.runtime");
const globalWithRuntime = globalThis as unknown as { [runtimeKey]: Runtime };

export const team = (globalWithRuntime[runtimeKey] ??= new Runtime());
