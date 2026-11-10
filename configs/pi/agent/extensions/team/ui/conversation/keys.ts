import { matchesKey } from "@earendil-works/pi-tui";

export type ConversationAction =
	| "close"
	| "up"
	| "down"
	| "pageUp"
	| "pageDown"
	| "home"
	| "end"
	| "tools"
	| "markdown"
	| "none";

type Binding =
	| "tui.select.up"
	| "tui.select.down"
	| "tui.select.pageUp"
	| "tui.select.pageDown"
	| "tui.select.cancel"
	| "app.tools.expand";
export type ConversationKeys = { matches: (data: string, action: Binding) => boolean };

export function conversationKeyAction(data: string, keys: ConversationKeys): ConversationAction {
	const bound = (action: Binding): boolean => {
		try {
			return typeof keys?.matches === "function" && keys.matches(data, action);
		} catch {
			return false;
		}
	};
	if (
		bound("tui.select.cancel") ||
		matchesKey(data, "escape") ||
		matchesKey(data, "ctrl+c") ||
		data === "q"
	)
		return "close";
	if (bound("tui.select.up") || matchesKey(data, "up") || data === "k") return "up";
	if (bound("tui.select.down") || matchesKey(data, "down") || data === "j") return "down";
	if (bound("tui.select.pageUp") || matchesKey(data, "pageUp")) return "pageUp";
	if (bound("tui.select.pageDown") || matchesKey(data, "pageDown")) return "pageDown";
	if (matchesKey(data, "home")) return "home";
	if (matchesKey(data, "end")) return "end";
	if (bound("app.tools.expand") || matchesKey(data, "ctrl+o") || data === "t") return "tools";
	if (data === "m") return "markdown";
	return "none";
}
