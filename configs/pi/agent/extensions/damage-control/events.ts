import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export const DAMAGE_CONTROL_ACTION_BLOCKED_EVENT = "damage-control:action:blocked";
export const DAMAGE_CONTROL_RISK_DETECTED_EVENT = "damage-control:risk:detected";
export const DAMAGE_CONTROL_FEATURE_REQUEST_EVENT = "damage-control:feature:request";
export const DAMAGE_CONTROL_FEATURE_REGISTER_EVENT = "damage-control:feature:register";

export type DamageControlFeatureId = "commands" | "paths" | "policies";
export type DamageControlActionKind = "tool_call" | "user_bash";
export type DamageControlBlockSource =
	| "policy"
	| "pathAccess"
	| "user"
	| "nonInteractive";

export type DamageControlAction = {
	kind: DamageControlActionKind;
	toolName?: string;
	command?: string;
	path?: string;
};

export type DamageControlRisk<TMeta = unknown> = {
	kind: "dangerous";
	reason: string;
	metadata?: TMeta;
};

export type DamageControlEventBase = {
	source: "damage-control";
	feature: DamageControlFeatureId;
	timestamp: string;
};

export type DamageControlFeatureRequestPayload = {
	source: "damage-control";
	timestamp: string;
};

export type DamageControlFeatureRegisterPayload = {
	source: "damage-control";
	timestamp: string;
	feature: {
		id: DamageControlFeatureId;
	};
};

export type DamageControlActionBlockedPayload<TMeta = unknown> =
	DamageControlEventBase & {
		action: DamageControlAction;
		reason: string;
		block: {
			source: DamageControlBlockSource;
			metadata?: TMeta;
		};
		context?: {
			toolName?: string;
			input?: Record<string, unknown>;
		};
	};

export type DamageControlRiskDetectedPayload<TMeta = unknown> =
	DamageControlEventBase & {
		risk: DamageControlRisk<TMeta>;
		context?: {
			toolName?: string;
			input?: Record<string, unknown>;
		};
	};

function timestamp(): string {
	return new Date().toISOString();
}

export function createFeatureRequestPayload(): DamageControlFeatureRequestPayload {
	return {
		source: "damage-control",
		timestamp: timestamp(),
	};
}

export function createFeatureRegisterPayload(
	feature: DamageControlFeatureId,
): DamageControlFeatureRegisterPayload {
	return {
		source: "damage-control",
		timestamp: timestamp(),
		feature: { id: feature },
	};
}

export function emitFeatureRegister(
	pi: ExtensionAPI,
	feature: DamageControlFeatureId,
): void {
	pi.events.emit(
		DAMAGE_CONTROL_FEATURE_REGISTER_EVENT,
		createFeatureRegisterPayload(feature),
	);
}

export function emitActionBlocked<TMeta = unknown>(
	pi: ExtensionAPI,
	event: Omit<DamageControlActionBlockedPayload<TMeta>, "source" | "timestamp">,
): void {
	pi.events.emit(DAMAGE_CONTROL_ACTION_BLOCKED_EVENT, {
		source: "damage-control",
		timestamp: timestamp(),
		...event,
	});
}

export function emitRiskDetected<TMeta = unknown>(
	pi: ExtensionAPI,
	event: Omit<DamageControlRiskDetectedPayload<TMeta>, "source" | "timestamp">,
): void {
	pi.events.emit(DAMAGE_CONTROL_RISK_DETECTED_EVENT, {
		source: "damage-control",
		timestamp: timestamp(),
		...event,
	});
}
