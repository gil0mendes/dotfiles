import type { EventBus, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { isTeamRuntimeChangedPayload, TEAM_RUNTIME_CHANGED_EVENT } from "../events";
import { RECENT_AGENT_TTL_MS, type Runtime } from "../runtime";
import { createConversationOverlay } from "./conversation/overlay";
import { createFleetOverlay, type FleetOverlay } from "./overlay";
import { sanitizeFleetText } from "./presentation";
import { createFleetState } from "./state";
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
	let dialogAbort: AbortController | undefined;
	let closeConversationResources: (() => void) | undefined;
	let closeFleetResources: (() => void) | undefined;
	let generation = 0;

	const showConversation = async (
		ctx: FleetCommandContext,
		ownerSessionId: string,
		agentId: string,
		currentGeneration: number,
	): Promise<void> => {
		let overlay: ReturnType<typeof createConversationOverlay> | undefined;
		let expiryTimer: ReturnType<typeof setTimeout> | undefined;
		let closed = false;
		const snapshot = () =>
			generation === currentGeneration &&
			!closed &&
			ctx.sessionManager.getSessionId() === ownerSessionId
				? runtime.getConversationForOwner(ownerSessionId, agentId)
				: undefined;
		const refresh = (): void => {
			if (closed || generation !== currentGeneration) return;
			overlay?.refresh();
		};
		const scheduleExpiry = (expiresAt: number | undefined): void => {
			if (expiryTimer) clearTimeout(expiryTimer);
			expiryTimer = undefined;
			if (!closed && expiresAt !== undefined)
				expiryTimer = setTimeout(refresh, Math.max(0, expiresAt - Date.now()) + 1);
		};
		const unsubscribe = runtime.subscribeConversation(ownerSessionId, agentId, refresh);
		const cleanup = (): void => {
			if (closed) return;
			closed = true;
			if (expiryTimer) clearTimeout(expiryTimer);
			unsubscribe();
		};
		closeConversationResources = cleanup;
		try {
			let mounted = false;
			await ctx.ui.custom<void>(
				(tui, _theme, keybindings, done) => {
					if (generation !== currentGeneration || closed) {
						done();
						return { render: () => [], invalidate: () => {} };
					}
					mounted = true;
					closeOverlay = () => done();
					overlay = createConversationOverlay(tui, keybindings, snapshot, done, (current) =>
						scheduleExpiry(current?.expiresAt),
					);
					refresh();
					return overlay;
				},
				{ overlay: true, overlayOptions: { width: "90%", maxHeight: "80%", anchor: "center" } },
			);
			if (!mounted && generation === currentGeneration)
				ctx.ui.notify("Conversation viewer is unavailable in this interface.", "warning");
		} catch {
			if (generation === currentGeneration)
				ctx.ui.notify("Conversation viewer is unavailable in this interface.", "warning");
		} finally {
			cleanup();
			if (closeConversationResources === cleanup) closeConversationResources = undefined;
			if (generation === currentGeneration) closeOverlay = undefined;
		}
	};

	const awaitDialog = async <T>(
		abort: AbortController,
		request: () => Promise<T>,
	): Promise<T | undefined> => {
		dialogAbort = abort;
		try {
			return await request();
		} catch (error) {
			if (abort.signal.aborted) return undefined;
			throw error;
		} finally {
			if (dialogAbort === abort) dialogAbort = undefined;
		}
	};

	const handleAction = async (
		ctx: FleetCommandContext,
		ownerSessionId: string,
		action: FleetAction,
		currentGeneration: number,
	): Promise<void> => {
		if (action.kind === "close" || action.kind === "view" || generation !== currentGeneration)
			return;
		const agent = runtime
			.getActiveSummariesForOwner(ownerSessionId)
			.find((candidate) => candidate.id === action.agentId);
		if (!agent) {
			ctx.ui.notify("Selected agent is no longer active in this session.", "warning");
			return;
		}
		const agentName = sanitizeFleetText(agent.agentName) || "unnamed agent";
		if (action.kind === "respond") {
			if (agent.status !== "waiting") {
				ctx.ui.notify("Selected agent is no longer waiting for a response.", "warning");
				return;
			}
			const abort = new AbortController();
			const response = await awaitDialog(abort, () =>
				ctx.ui.input(`Response for ${agentName}`, "Message", { signal: abort.signal }),
			);
			if (generation !== currentGeneration || ctx.sessionManager.getSessionId() !== ownerSessionId)
				return;
			const message = response?.trim();
			if (!message) return;
			const result = runtime.respond(agent.id, message, ownerSessionId);
			ctx.ui.notify(
				result.error ? sanitizeFleetText(result.error) : `Message sent to ${agentName}.`,
				result.error ? "warning" : "info",
			);
			return;
		}
		const abort = new AbortController();
		const confirmed = await awaitDialog(abort, () =>
			ctx.ui.confirm("Abort team agent?", `Abort ${agentName}? This cannot be undone.`, {
				signal: abort.signal,
			}),
		);
		if (generation !== currentGeneration || ctx.sessionManager.getSessionId() !== ownerSessionId)
			return;
		if (!confirmed) return;
		const aborted = runtime.abortOwned(agent.id, ownerSessionId, {
			reason: "Aborted from /team fleet",
		});
		ctx.ui.notify(
			aborted ? `Aborted ${agentName}.` : "Selected agent is no longer active in this session.",
			aborted ? "info" : "warning",
		);
	};

	const show = async (ctx: FleetCommandContext): Promise<void> => {
		if (open) return open;
		const ownerSessionId = ctx.sessionManager.getSessionId();
		const currentGeneration = generation;
		const operation = (async () => {
			let overlay: FleetOverlay | undefined;
			let expiryTimer: ReturnType<typeof setTimeout> | undefined;
			const refresh = (): void => {
				if (generation !== currentGeneration) return;
				overlay?.refresh();
				if (expiryTimer) clearTimeout(expiryTimer);
				const delay = nextExpiryDelay(runtime, ownerSessionId, Date.now());
				if (delay !== undefined) expiryTimer = setTimeout(refresh, delay + 1);
			};
			const unsubscribe = events.on(TEAM_RUNTIME_CHANGED_EVENT, (event) => {
				if (!isTeamRuntimeChangedPayload(event) || event.ownerSessionId !== ownerSessionId) return;
				refresh();
			});
			let cleaned = false;
			const cleanup = (): void => {
				if (cleaned) return;
				cleaned = true;
				if (expiryTimer) clearTimeout(expiryTimer);
				unsubscribe();
			};
			closeFleetResources = cleanup;
			try {
				let fleetState = createFleetState();
				while (
					generation === currentGeneration &&
					ctx.sessionManager.getSessionId() === ownerSessionId
				) {
					let didMountOverlay = false;
					let action: FleetAction | undefined;
					try {
						action = await ctx.ui.custom<FleetAction>(
							(tui, _theme, keybindings, done) => {
								if (
									generation !== currentGeneration ||
									ctx.sessionManager.getSessionId() !== ownerSessionId
								) {
									done({ kind: "close" });
									return { render: () => [], invalidate: () => {} };
								}
								didMountOverlay = true;
								closeOverlay = () => done({ kind: "close" });
								overlay = createFleetOverlay(
									tui,
									keybindings,
									() =>
										generation === currentGeneration &&
										ctx.sessionManager.getSessionId() === ownerSessionId
											? getFleetView(runtime, ownerSessionId)
											: { rows: [], detachedActiveCount: 0 },
									done,
									Date.now,
									fleetState,
									(state) => {
										fleetState = state;
									},
									() =>
										generation === currentGeneration &&
										ctx.sessionManager.getSessionId() === ownerSessionId,
								);
								refresh();
								return overlay;
							},
							{
								overlay: true,
								overlayOptions: { width: "90%", maxHeight: "80%", anchor: "center" },
							},
						);
					} catch {
						if (generation !== currentGeneration) return;
						ctx.ui.notify(
							"Fleet overlay is unavailable in this interface. Use /team list for the roster.",
							"warning",
						);
						return;
					}
					if (
						generation !== currentGeneration ||
						ctx.sessionManager.getSessionId() !== ownerSessionId
					)
						return;
					if (!didMountOverlay) {
						ctx.ui.notify(
							"Fleet overlay is unavailable in this interface. Use /team list for the roster.",
							"warning",
						);
						return;
					}
					closeOverlay = undefined;
					overlay = undefined;
					if (action?.kind === "view") {
						await showConversation(ctx, ownerSessionId, action.agentId, currentGeneration);
						continue;
					}
					if (action) await handleAction(ctx, ownerSessionId, action, currentGeneration);
					break;
				}
			} finally {
				if (generation === currentGeneration) closeOverlay = undefined;
				cleanup();
				if (closeFleetResources === cleanup) closeFleetResources = undefined;
			}
		})();
		open = operation;
		try {
			await operation;
		} finally {
			if (open === operation) open = undefined;
		}
	};

	const dispose = (): void => {
		generation++;
		dialogAbort?.abort();
		dialogAbort = undefined;
		closeConversationResources?.();
		closeConversationResources = undefined;
		closeFleetResources?.();
		closeFleetResources = undefined;
		closeOverlay?.();
		closeOverlay = undefined;
		open = undefined;
	};

	return { show, dispose };
};
