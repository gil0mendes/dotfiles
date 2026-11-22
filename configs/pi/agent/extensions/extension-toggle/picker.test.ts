import { describe, expect, it, vi } from "vitest";
import { visibleWidth } from "@earendil-works/pi-tui";
import { TogglePicker } from "./picker.js";

function setup() {
	const options = [
		{ label: "Alpha", enabled: true },
		{ label: "Beta", enabled: false },
	];
	const done = vi.fn();
	const picker = new TogglePicker(
		options,
		{ fg: (_, text) => text },
		() => 1,
		() => {},
		done,
	);
	return { options, done, picker };
}

describe("TogglePicker", () => {
	it("keeps staged changes across searches and submits only differences", () => {
		const { options, done, picker } = setup();
		picker.handleInput(" ");
		picker.handleInput("b");
		picker.handleInput(" ");
		picker.handleInput(" ");
		picker.handleInput("\r");
		expect(done).toHaveBeenCalledWith([{ option: options[0], enabled: false }]);
	});

	it("clears the filter before cancelling, without submitting changes", () => {
		const { done, picker } = setup();
		picker.handleInput(" ");
		picker.handleInput("z");
		expect(picker.render(80).join("\n")).toContain("No matching resources");
		picker.handleInput("\x1b");
		expect(done).not.toHaveBeenCalled();
		expect(picker.render(80).join("\n")).toContain("Alpha");
		picker.handleInput("\x1b");
		expect(done).toHaveBeenCalledWith(null);
	});

	it("scrolls to the selected row and fits narrow terminals", () => {
		const { picker } = setup();
		picker.handleInput("\x1b[B");
		expect(picker.render(80).join("\n")).toContain("> [ ] Beta");
		expect(picker.render(3).every((line) => visibleWidth(line) <= 3)).toBe(true);
	});
});
