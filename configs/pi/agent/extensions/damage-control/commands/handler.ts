import {
	isToolCallEventType,
	type ToolCallEvent,
	type ToolCallEventResult,
} from "@earendil-works/pi-coding-agent";
import { configs } from "../config";
import { checkAction } from "../check";
import { createPermissionGateRule } from "./rules";

export function handleCommand(event: ToolCallEvent): ToolCallEventResult {
	if (!configs.current.permissionGate) {
		return {};
	}

	if (!isToolCallEventType("bash", event)) {
		return {};
	}

	const { command } = event.input;
	const action = { kind: "command" as const, command, origin: "bash" };

	const safety = checkAction(action, [
		createPermissionGateRule({
			patterns: configs.current.bashToolPatterns,
			useBuiltinMatchers: configs.current.useBuiltinCommandMatchers ?? true,
		}),
	]);

	if (safety.kind === "safe") return {};
	return { block: true, reason: safety.reason };
}
