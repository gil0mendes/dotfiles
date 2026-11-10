import test from "node:test";
import assert from "node:assert/strict";
import type { KeybindingHints } from "@earendil-works/pi-coding-agent";
import { appendExpandHint } from "../render.ts";
import { createWebFetchTool } from "../webfetch.ts";
import { createWebSearchTool } from "../websearch.ts";

function hints(marker: string) {
	return {
		marker,
		calls: [] as Array<[string, string]>,
		keyHint(action, description) {
			this.calls.push([action, description]);
			return `${this.marker} ${description}`;
		},
	} satisfies Pick<KeybindingHints, "keyHint"> & { marker: string; calls: Array<[string, string]> };
}

const theme = { fg: (_name: string, value: string) => value };
const collapsed = { expanded: false, isPartial: false };
const expanded = { expanded: true, isPartial: false };
const partial = { expanded: false, isPartial: true };
const text = (component: { render(width: number): string[] }) => component.render(1000).join("\n");
const lines = Array.from({ length: 18 }, (_, index) => `line-${index + 1}`).join("\n");
const fetchDetails = {
	requestedUrl: "https://example.test/start",
	finalUrl: "https://example.test/final",
	format: "text" as const,
	status: 200,
	mime: "text/plain",
	contentType: "text/plain",
	bytes: 42,
	truncated: true,
	fullOutputPath: "/fixture/fetch.txt",
};
const searchDetails = {
	query: "fixture",
	depth: "auto" as const,
	maxResults: 8,
	provider: "exa" as const,
	resultCount: 2,
	results: [],
	truncated: true,
	fullOutputPath: "/fixture/search.txt",
};

test("appendExpandHint uses the injected receiver only when collapsed", () => {
	const binding = hints("REMAP[alt+shift+d]");
	assert.equal(appendExpandHint("base", true, binding), "base");
	assert.deepEqual(binding.calls, []);
	assert.equal(appendExpandHint("base", false, binding), "base REMAP[alt+shift+d] for details");
	assert.deepEqual(binding.calls, [["app.tools.expand", "for details"]]);
});

for (const fixture of [
	{
		name: "webfetch",
		render: createWebFetchTool().renderResult,
		details: { ...searchDetails, ...fetchDetails },
		limit: 12,
		progress: "Fetching...",
		path: fetchDetails.fullOutputPath,
	},
	{
		name: "websearch",
		render: createWebSearchTool().renderResult,
		details: { ...fetchDetails, ...searchDetails },
		limit: 16,
		progress: "Searching...",
		path: searchDetails.fullOutputPath,
	},
]) {
	test(`${fixture.name} uses per-render hints and preserves previews and paths`, () => {
		const result = { content: [{ type: "text", text: lines }], details: fixture.details };
		const first = hints("REMAP[alt+shift+d]");
		const second = hints("REMAP[super+x]");
		const firstOutput = text(fixture.render(result, collapsed, theme, first));
		assert.match(firstOutput, /REMAP\[alt\+shift\+d\] for details/);
		assert.match(firstOutput, /\[truncated\]/);
		assert.doesNotMatch(firstOutput, /ctrl\+o|Full output:/i);
		assert.deepEqual(first.calls, [["app.tools.expand", "for details"]]);
		assert.match(
			text(fixture.render(result, collapsed, theme, second)),
			/REMAP\[super\+x\] for details/,
		);
		assert.deepEqual(second.calls, [["app.tools.expand", "for details"]]);
		const noHint = hints("UNEXPECTED");
		const output = text(fixture.render(result, expanded, theme, noHint));
		assert.match(output, new RegExp(`line-${fixture.limit}(?:\\s|$)`));
		assert.doesNotMatch(output, new RegExp(`line-${fixture.limit + 1}(?:\\s|$)`));
		assert.match(output, /\.\.\./);
		assert.ok(output.includes(`Full output: ${fixture.path}`));
		const longOutput = text(
			fixture.render(
				{ content: [{ type: "text", text: "x".repeat(240) }], details: fixture.details },
				expanded,
				theme,
				noHint,
			),
		);
		assert.ok(longOutput.includes("x".repeat(220)));
		assert.ok(!longOutput.includes("x".repeat(221)));
		assert.deepEqual(noHint.calls, []);
	});

	test(`${fixture.name} partial and error results do not request hints`, () => {
		const binding = hints("UNEXPECTED");
		assert.ok(
			text(fixture.render({ content: [] }, partial, theme, binding)).includes(fixture.progress),
		);
		assert.ok(
			text(
				fixture.render(
					{ content: [{ type: "text", text: "synthetic failure" }], isError: true },
					collapsed,
					theme,
					binding,
				),
			).includes("✗ synthetic failure"),
		);
		assert.deepEqual(binding.calls, []);
	});
}

test("webfetch image marker and expanded URL remain visible", () => {
	const binding = hints("REMAP[alt+shift+d]");
	const result = { content: [{ type: "image" }], details: { ...fetchDetails, image: true } };
	const render = createWebFetchTool().renderResult;
	assert.match(text(render(result, collapsed, theme, binding)), /\[image\].*REMAP/);
	const expandedHints = hints("UNEXPECTED");
	const output = text(render(result, expanded, theme, expandedHints));
	assert.ok(output.includes("Image URL: https://example.test/final"));
	assert.ok(output.includes("Full output: /fixture/fetch.txt"));
	assert.deepEqual(expandedHints.calls, []);
});
