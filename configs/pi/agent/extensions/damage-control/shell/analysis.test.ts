import { describe, expect, it } from "vitest";
import { DEFAULT_CONFIG } from "../config";
import { evaluateBash } from "./analysis";

describe("evaluateBash", () => {
	it("skips command checks when permissionGate is disabled", () => {
		expect(
			evaluateBash("sudo rm -rf /tmp/example", {
				...DEFAULT_CONFIG,
				permissionGate: false,
			}),
		).toBeNull();
	});
});
