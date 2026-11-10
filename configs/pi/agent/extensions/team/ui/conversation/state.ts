import type { ConversationAction } from "./keys";

export type ConversationState = {
	offset: number;
	follow: boolean;
	expanded: boolean;
	markdown: boolean;
};
export const createConversationState = (): ConversationState => ({
	offset: 0,
	follow: true,
	expanded: false,
	markdown: true,
});

export function positionConversation(
	state: ConversationState,
	count: number,
	height: number,
): ConversationState {
	const bottom = Math.max(0, count - height);
	return { ...state, offset: state.follow ? bottom : Math.min(state.offset, bottom) };
}

export function moveConversation(
	state: ConversationState,
	action: ConversationAction,
	count: number,
	height: number,
): ConversationState {
	const current = positionConversation(state, count, height);
	const bottom = Math.max(0, count - height);
	if (action === "tools") return { ...current, expanded: !current.expanded };
	if (action === "markdown") return { ...current, markdown: !current.markdown };
	if (action === "end") return { ...current, follow: true, offset: bottom };
	if (action === "home") return { ...current, follow: false, offset: 0 };
	const delta =
		action === "up"
			? -1
			: action === "down"
				? 1
				: action === "pageUp"
					? -height
					: action === "pageDown"
						? height
						: 0;
	if (!delta) return current;
	return {
		...current,
		follow: false,
		offset: Math.max(0, Math.min(bottom, current.offset + delta)),
	};
}
