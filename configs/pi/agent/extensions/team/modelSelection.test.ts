import { describe, expect, it } from "vitest";
import { parseModel } from "./loader/utils";
import { selectAvailableModel } from "./modelSelection";

const select = (
	query: string,
	models: readonly { provider: string; id: string; name?: string }[],
) => {
	const selector = parseModel(query);
	if (!selector) throw new Error(`Invalid selector: ${query}`);
	return selectAvailableModel(selector, models);
};
const model = (id: string, name?: string, provider = "p") => ({ provider, id, name });

describe("selectAvailableModel", () => {
	it.each([
		["FOO-BAR", model("foo-bar"), model("foo-bar-extra")],
		["Foo Bar", model("opaque", "Foo Bar"), model("foo-bar-extra")],
		["foo bar", model("foo-bar"), model("foo-bar-extra")],
		["foo bar", model("opaque", "foo-bar"), model("unrelated", "foo bar extra")],
		["foo ba", model("foo-bar-extra"), model("other-foo-ba")],
		["oo-ba", model("foo-bar"), model("opaque", "oo ba extra")],
		["oo-ba", model("opaque", "foo-bar"), model("bar", "foo")],
		["bar foo", model("foo-x", "bar"), model("unrelated")],
	])("ranks bare query %s", (query, winner, loser) => {
		expect(select(query, [loser, winner])).toEqual({ kind: "matched", model: winner });
	});

	it("keeps equal scores ambiguous regardless of ordering or dates", () => {
		const models = [model("claude-sonnet-2024"), model("claude-sonnet-2025")];
		expect(select("claude-sonnet", models).kind).toBe("ambiguous");
		expect(select("claude-sonnet", [...models].reverse()).kind).toBe("ambiguous");
		expect(select("foo", [model("foo"), model("foo", undefined, "q")]).kind).toBe("ambiguous");
	});

	it("does not match providers, wildcard syntax, or empty normalization vacuously", () => {
		expect(select("provider", [model("foo", undefined, "provider")]).kind).toBe("unavailable");
		expect(select("foo*", [model("foo-bar")]).kind).toBe("unavailable");
		expect(select("---", [model("foo")]).kind).toBe("unavailable");
		expect(select("---", [model("---")]).kind).toBe("matched");
		expect(selectAvailableModel({ kind: "bare", raw: "", query: "" }, [model("")]).kind).toBe(
			"unavailable",
		);
	});

	it("does not reward multiple weaker rules, token skips, or apparent recency", () => {
		const prefix = model("foo-bar-extra");
		const distributed = model("foo-x-bar", "foo bar extra");
		expect(select("foo ba", [distributed, prefix])).toEqual({ kind: "matched", model: prefix });
		expect(select("foo bar", [model("foo-x-bar"), model("bar-x-foo")]).kind).toBe("ambiguous");
		expect(select("gpt-2024", [model("gpt-2025")]).kind).toBe("unavailable");
		expect(select("foo:bar", [model("foo-bar")]).kind).toBe("unavailable");
		expect(select("café", [model("cafe")]).kind).toBe("unavailable");
	});

	it("preserves qualified exact priority, provider binding and legacy skipped tokens", () => {
		const literal = model("foo");
		expect(select("p/foo", [model("FOO"), literal])).toEqual({ kind: "matched", model: literal });
		expect(select("p/FoO", [model("FOO"), literal]).kind).toBe("ambiguous");
		expect(select("P/cla-op", [model("claude-4-opus")]).kind).toBe("matched");
		expect(
			select("OPENROUTER/anthropic/claude", [model("anthropic/claude", undefined, "openrouter")])
				.kind,
		).toBe("matched");
		expect(select("p/foo:ba", [model("foo-x-bar")]).kind).toBe("matched");
		expect(select("p/foo", [model("foo", undefined, "q")]).kind).toBe("unavailable");
		expect(select("open_ai/foo", [model("foo", undefined, "open-ai")]).kind).toBe("unavailable");
		expect(select("p/Display Name", [model("opaque", "Display Name")]).kind).toBe("unavailable");
		expect(
			selectAvailableModel({ kind: "qualified", raw: "p/---", provider: "p", modelId: "---" }, [
				model("foo"),
			]).kind,
		).toBe("unavailable");
	});

	it("does not apply bare ranking to qualified fuzzy results", () => {
		const models = [model("foo-bar"), model("foo-x-bar")];
		expect(select("p/foo.bar", models).kind).toBe("ambiguous");
		expect(select("foo.bar", models)).toEqual({ kind: "matched", model: models[0] });
	});

	it("deduplicates literal identities and returns the original object without mutation", () => {
		const first = model("foo");
		const duplicate = model("foo", "other");
		const input = [first, duplicate];
		const result = select("foo", input);
		expect(result.kind === "matched" && result.model).toBe(first);
		expect(input).toEqual([first, duplicate]);
	});
});
