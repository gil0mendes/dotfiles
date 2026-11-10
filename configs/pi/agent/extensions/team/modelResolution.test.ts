import { describe, expect, it } from "vitest";
import { parseModel } from "./loader/utils";
import { resolveModelPreferences } from "./subagentRunner";
import type { ParsedModel } from "./loader/types";

const selector = (value: string): ParsedModel => {
	const parsed = parseModel(value);
	if (!parsed) throw new Error(`Invalid test selector: ${value}`);
	return parsed;
};

const model = (provider: string, id: string) => ({ provider, id });

describe("resolveModelPreferences", () => {
	it("uses ordered bare matches after neutral warnings and preserves raw selector spelling", () => {
		const chosen = { provider: "q", id: "opaque", name: "Sonnet model" };
		const result = resolveModelPreferences(
			[selector(" missing "), selector("  Sonnet  "), selector("q/exact")],
			[chosen, model("q", "exact")],
		);
		expect(result.model).toBe(chosen);
		expect(result.warnings).toEqual([
			'Model selector "missing" is unavailable, skipping',
			'Model selector "Sonnet" resolved to "q/opaque"',
		]);
	});

	it("skips bare ambiguity before a later qualified exact preference", () => {
		const result = resolveModelPreferences(
			[selector("foo"), selector("q/foo")],
			[model("q", "foo"), model("p", "foo")],
		);
		expect(result.model).toEqual(model("q", "foo"));
		expect(result.warnings).toEqual([
			'Model selector "foo" is ambiguous among "p/foo", "q/foo", skipping',
		]);
	});
	it("keeps exact provider/id matches case-sensitive and ahead of fuzzy matching", () => {
		const result = resolveModelPreferences(
			[selector("openai/gpt-5")],
			[model("openai", "gpt-5-mini"), model("openai", "gpt-5")],
		);

		expect(result).toEqual({ model: model("openai", "gpt-5"), warnings: [] });
	});

	it("selects a unique provider-qualified fuzzy model match", () => {
		const result = resolveModelPreferences(
			[selector("anthropic/cla-op")],
			[model("anthropic", "claude-4-opus"), model("openai", "claude-4-opus")],
		);

		expect(result.model).toEqual(model("anthropic", "claude-4-opus"));
		expect(result.warnings).toEqual([
			'Model selector "anthropic/cla-op" resolved to "anthropic/claude-4-opus"',
		]);
	});

	it("matches query tokens as ordered prefixes across slash, dash, dot, underscore, and colon", () => {
		const result = resolveModelPreferences(
			[selector("provider/foo/bar-baz.qux_quux:corge")],
			[model("provider", "foo/bar-baz.qux_quux:corge-v2")],
		);

		expect(result.model).toEqual(model("provider", "foo/bar-baz.qux_quux:corge-v2"));
	});

	it("does not cross providers or resolve ambiguous matches, then tries later preferences", () => {
		const result = resolveModelPreferences(
			[selector("one/gpt"), selector("two/son")],
			[
				model("one", "gpt-4"),
				model("one", "gpt-5"),
				model("two", "sonnet-4"),
				model("other", "sonnet-4"),
			],
		);

		expect(result.model).toEqual(model("two", "sonnet-4"));
		expect(result.warnings).toEqual([
			'Model selector "one/gpt" is ambiguous among "one/gpt-4", "one/gpt-5", skipping',
			'Model selector "two/son" resolved to "two/sonnet-4"',
		]);
	});

	it("deduplicates identities and caps safely escaped ambiguity warnings", () => {
		const result = resolveModelPreferences(
			[selector('provider/gpt\\"')],
			[
				model("provider", 'gpt\\"-one'),
				model("provider", 'gpt\\"-one'),
				model("provider", 'gpt\\"-two'),
				model("provider", 'gpt\\"-three'),
				model("provider", 'gpt\\"-four'),
			],
		);

		expect(result.model).toBeUndefined();
		expect(result.warnings).toHaveLength(1);
		expect(result.warnings[0]).toContain("… 1 more");
		expect(result.warnings[0]).toContain('\\"');
	});

	it("formats ambiguity warnings independently of registry ordering", () => {
		const preferences = [selector("provider/gpt")];
		const forward = resolveModelPreferences(preferences, [
			model("provider", "gpt-4"),
			model("provider", "gpt-5"),
		]);
		const reverse = resolveModelPreferences(preferences, [
			model("provider", "gpt-5"),
			model("provider", "gpt-4"),
		]);

		expect(forward.warnings).toEqual(reverse.warnings);
	});

	it("sorts canonical labels before JSON escaping and retains internal selector spaces", () => {
		const result = resolveModelPreferences(
			[selector(" p / foo ")],
			[model("p", "foo\\x"), model("p", 'foo"x')],
		);
		expect(result.warnings).toEqual([
			`Model selector ${JSON.stringify("p / foo")} is ambiguous among ${JSON.stringify('p/foo"x')}, ${JSON.stringify("p/foo\\x")}, skipping`,
		]);
	});

	it("returns no resolution when every preference is unavailable", () => {
		expect(resolveModelPreferences([selector("provider/gpt")], [model("other", "gpt-5")])).toEqual({
			warnings: ['Model selector "provider/gpt" is unavailable, skipping'],
		});
	});
});
