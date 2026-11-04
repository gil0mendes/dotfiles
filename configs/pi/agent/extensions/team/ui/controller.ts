import type { EventBus, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { isTeamRuntimeChangedPayload, TEAM_RUNTIME_CHANGED_EVENT } from "../events";
import { RECENT_AGENT_TTL_MS, type Runtime } from "../runtime";
import { createFleetOverlay, type FleetOverlay } from "./overlay";
import { getFleetView } from "./source";
import type { FleetAction } from "./types";

type FleetCommandContext = {
	sessionManager: Pick<ExtensionCommandContext["sessionManager"], "getSessionId">;
	ui: Pick<ExtensionCommandContext["ui"], "confirm" | "custom" | "input" | "notify">;
};

const nextExpiryDelay = (
	runtime: Runtime,
	ownerSessionId: string,
	now: number,
): number | undefined => {
	const recent = runtime.getFleetSnapshotForOwner(ownerSessionId).recent;
	if (recent.length === 0) return undefined;
	const nearestExpiry = Math.min(...recent.map((agent) => agent.settledAt + RECENT_AGENT_TTL_MS));
	return Math.max(0, nearestExpiry - now);
};

export const createFleetController = (events: EventBus, runtime: Runtime) => {
	let open: Promise<void> | undefined;
	let closeOverlay: (() => void) | undefined;

	const handleAction = async (
		ctx: FleetCommandContext,
		ownerSessionId: string,
		action: FleetAction,
	): Promise<void> => {
		if (action.kind === "close") return;
		const agent = runtime
			.getActiveSummariesForOwner(ownerSessionId)
			.find((candidate) => candidate.id === action.agentId);
		if (!agent) {
			ctx.ui.notify("Selected agent is no longer active in this session.", "warning");
			return;
		}
		if (action.kind === "respond") {
			if (agent.status !== "waiting") {
				ctx.ui.notify("Selected agent is no longer waiting for a response.", "warning");
				return;
			}
			const response = await ctx.ui.input(`Response for ${agent.agentName}`, "Message");
			const message = response?.trim();
			if (!message) return;
			const result = runtime.respond(agent.id, message, ownerSessionId);
			ctx.ui.notify(
				result.error ?? `Message sent to ${agent.agentName}.`,
				result.error ? "warning" : "info",
			);
			return;
		}
		const confirmed = await ctx.ui.confirm(
			"Abort team agent?",
			`Abort ${agent.agentName}? This cannot be undone.`,
		);
		if (!confirmed) return;
		const aborted = runtime.abortOwned(agent.id, ownerSessionId, {
			reason: "Aborted from /team fleet",
		});
		ctx.ui.notify(
			aborted
				? `Aborted ${agent.agentName}.`
				: "Selected agent is no longer active in this session.",
			aborted ? "info" : "warning",
		);
	};

	const show = async (ctx: FleetCommandContext): Promise<void> => {
		if (open) return open;
		const ownerSessionId = ctx.sessionManager.getSessionId();
		open = (async () => {
			let overlay: FleetOverlay | undefined;
			let expiryTimer: ReturnType<typeof setTimeout> | undefined;
			const refresh = (): void => {
				overlay?.refresh();
				if (expiryTimer) clearTimeout(expiryTimer);
				const delay = nextExpiryDelay(runtime, ownerSessionId, Date.now());
				if (delay !== undefined) expiryTimer = setTimeout(refresh, delay + 1);
			};
			const unsubscribe = events.on(TEAM_RUNTIME_CHANGED_EVENT, (event) => {
				if (!isTeamRuntimeChangedPayload(event) || event.ownerSessionId !== ownerSessionId) return;
				refresh();
			});
			try {
				let didMountOverlay = false;
				let action: FleetAction | undefined;
				try {
					action = await ctx.ui.custom<FleetAction>(
						(tui, _theme, keybindings, done) => {
							didMountOverlay = true;
							closeOverlay = () => done({ kind: "close" });
							overlay = createFleetOverlay(
								tui,
								keybindings,
								() => getFleetView(runtime, ownerSessionId),
								done,
							);
							refresh();
							return overlay;
						},
						{ overlay: true, overlayOptions: { width: "90%", maxHeight: "80%", anchor: "center" } },
					);
				} catch {
					ctx.ui.notify(
						"Fleet overlay is unavailable in this interface. Use /team list for the roster.",
						"warning",
					);
					return;
				}
				if (!didMountOverlay) {
					ctx.ui.notify(
						"Fleet overlay is unavailable in this interface. Use /team list for the roster.",
						"warning",
					);
					return;
				}
				if (action) await handleAction(ctx, ownerSessionId, action);
			} finally {
				closeOverlay = undefined;
				if (expiryTimer) clearTimeout(expiryTimer);
				unsubscribe();
			}
		})();
		try {
			await open;
		} finally {
			open = undefined;
		}
	};

	const dispose = (): void => {
		closeOverlay?.();
	};

	return { show, dispose };
};
