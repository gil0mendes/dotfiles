import { matchesKey } from "@earendil-works/pi-tui";

export type FleetKeyAction = "up" | "down" | "view" | "respond" | "abort" | "close" | "none";
type FleetBinding =
	| "tui.select.up"
	| "tui.select.down"
	| "tui.select.cancel"
	| "tui.select.confirm";

export const fleetKeyAction = (
	data: string,
	matches: (data: string, action: FleetBinding) => boolean,
): FleetKeyAction => {
	const bound = (action: FleetBinding): boolean => {
		try {
			return matches(data, action);
		} catch {
			return false;
		}
	};
	if (bound("tui.select.up") || matchesKey(data, "up") || data === "k") return "up";
	if (bound("tui.select.down") || matchesKey(data, "down") || data === "j") return "down";
	if (
		bound("tui.select.cancel") ||
		matchesKey(data, "escape") ||
		matchesKey(data, "ctrl+c") ||
		data === "q"
	)
		return "close";
	if (bound("tui.select.confirm") || matchesKey(data, "enter") || data === "v") return "view";
	if (data === "r") return "respond";
	if (data === "a") return "abort";
	return "none";
};
