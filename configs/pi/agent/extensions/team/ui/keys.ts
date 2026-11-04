export type FleetKeyAction = "up" | "down" | "respond" | "abort" | "close" | "none";
type FleetBinding = "tui.select.up" | "tui.select.down" | "tui.select.cancel";

export const fleetKeyAction = (
	data: string,
	matches: (data: string, action: FleetBinding) => boolean,
): FleetKeyAction => {
	if (matches(data, "tui.select.up") || data === "k") return "up";
	if (matches(data, "tui.select.down") || data === "j") return "down";
	if (matches(data, "tui.select.cancel")) return "close";
	if (data === "r") return "respond";
	if (data === "a") return "abort";
	return "none";
};
