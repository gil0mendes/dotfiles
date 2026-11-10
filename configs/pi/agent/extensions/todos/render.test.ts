import { describe, expect, it } from "vitest";
import type {
	ExtensionAPI,
	KeybindingHints,
	Theme,
	ToolRenderers,
} from "@earendil-works/pi-coding-agent";
import todosExtension from "./index.ts";

function captureRenderer() {
	let renderResult: ToolRenderers["renderResult"];
	const registration: Pick<ExtensionAPI, "on" | "registerCommand" | "registerTool"> = {
		on: () => () => {},
		registerCommand: () => {},
		registerTool(tool) {
			renderResult = tool.renderResult as ToolRenderers["renderResult"];
		},
	};
	// Only registration methods are used; no lifecycle, command, or tool is invoked.
	todosExtension(registration as ExtensionAPI);
	if (!renderResult) throw new Error("Todo renderer was not registered");
	return renderResult;
}

function context(marker: string) {
	const calls: Array<[string, string]> = [];
	return {
		marker,
		calls,
		args: {},
		toolCallId: "fixture",
		invalidate: () => {},
		lastComponent: undefined,
		state: undefined,
		cwd: "/fixture",
		executionStarted: true,
		argsComplete: true,
		isPartial: false,
		expanded: false,
		showImages: false,
		isError: false,
		durationMs: undefined,
		outputPad: 0,
		keyText: () => {
			throw new Error("keyText must not be called");
		},
		keyHint(action: Parameters<KeybindingHints["keyHint"]>[0], description: string) {
			this.calls.push([action, description]);
			return `${this.marker} ${description}`;
		},
	} satisfies Parameters<NonNullable<ToolRenderers["renderResult"]>>[3] & {
		marker: string;
		calls: Array<[string, string]>;
	};
}

// Rendering uses only fg; mark dim styling so the hint's styling can be checked.
const theme = {
	fg: (color: string, value: string) => (color === "dim" ? `<dim>${value}</dim>` : value),
} as unknown as Theme;
const todo = {
	id: "deadbeef",
	title: "Fixture task",
	tags: ["qa"],
	status: "open",
	created_at: "2026-01-01T00:00:00.000Z",
	assigned_to_session: "session-1",
	body: "Fixture body",
};
const closedTodo = { ...todo, id: "cafebabe", status: "closed", assigned_to_session: undefined };
const collapsed = { expanded: false, isPartial: false };
const expanded = { expanded: true, isPartial: false };
const render = captureRenderer();
function output(details: unknown, options: typeof collapsed, binding: ReturnType<typeof context>) {
	return render({ content: [{ type: "text", text: "fallback" }], details }, options, theme, binding)
		.render(1000)
		.join("\n");
}

describe("todo renderer hints", () => {
	it("uses receiver-sensitive remapped hints for detail and mutation results on each render", () => {
		const first = context("REMAP[alt+shift+d]");
		const detail = output({ action: "get", todo }, collapsed, first);
		expect(detail).toContain("TODO-deadbeef Fixture task");
		expect(detail).toContain("(assigned: session-1)");
		expect(detail).toContain("\n<dim>(REMAP[alt+shift+d] to expand)</dim>");
		expect(first.calls).toEqual([["app.tools.expand", "to expand"]]);
		const second = context("REMAP[super+x]");
		const mutation = output({ action: "update", todo }, collapsed, second);
		expect(mutation).toContain("✓ Updated TODO-deadbeef");
		expect(mutation).toContain("(REMAP[super+x] to expand)");
		expect(second.calls).toEqual([["app.tools.expand", "to expand"]]);
	});

	it("shows list hints only when collapsed lists include closed items", () => {
		for (const action of ["list", "list-all"]) {
			const binding = context("REMAP[list]");
			const result = output(
				{ action, todos: [todo, closedTodo], currentSessionId: "session-1" },
				collapsed,
				binding,
			);
			expect(result).toContain("assigned: session-1, current");
			expect(result).toContain("(REMAP[list] to expand)");
			expect(binding.calls).toEqual([["app.tools.expand", "to expand"]]);
		}
		for (const todos of [[todo], [todo, todo, todo, todo], []]) {
			const binding = context("UNEXPECTED");
			const result = output({ action: "list", todos }, collapsed, binding);
			expect(result).not.toContain("UNEXPECTED");
			if (todos.length === 0) expect(result).toContain("No todos");
			if (todos.length === 4) expect(result).toContain("... 1 more");
			expect(binding.calls).toEqual([]);
		}
	});

	it("does not hint expanded, partial, error, or fallback results", () => {
		const binding = context("UNEXPECTED");
		const detail = output({ action: "get", todo }, expanded, binding);
		expect(detail).toContain("Body:");
		expect(detail).toContain("Fixture body");
		expect(detail).toContain("assigned: session-1");
		expect(output({ action: "list-all", todos: [closedTodo] }, expanded, binding)).toContain(
			"TODO-cafebabe",
		);
		expect(
			output({ action: "get", todo }, { expanded: false, isPartial: true }, binding),
		).toContain("Processing...");
		expect(output({ action: "get", error: "missing" }, collapsed, binding)).toContain(
			"Error: missing",
		);
		expect(output(undefined, collapsed, binding)).toContain("fallback");
		expect(binding.calls).toEqual([]);
	});
});
