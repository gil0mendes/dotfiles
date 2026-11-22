import type { Theme } from "@earendil-works/pi-coding-agent";
import { Input, Key, matchesKey, truncateToWidth, type Focusable } from "@earendil-works/pi-tui";

export const TOGGLE_SHORTCUT = Key.ctrlShift("e");

export interface ToggleOption {
	label: string;
	enabled: boolean;
}

export interface ToggleChange<T> {
	option: T;
	enabled: boolean;
}

export class TogglePicker<T extends ToggleOption> implements Focusable {
	private readonly search = new Input();
	private readonly changes = new Map<T, boolean>();
	private cursor = 0;

	constructor(
		private readonly options: readonly T[],
		private readonly theme: Pick<Theme, "fg">,
		private readonly maxRows: () => number,
		private readonly redraw: () => void,
		private readonly done: (changes: ToggleChange<T>[] | null) => void,
	) {}

	get focused() {
		return this.search.focused;
	}
	set focused(value: boolean) {
		this.search.focused = value;
	}
	invalidate() {
		this.search.invalidate();
	}

	private filtered() {
		const query = this.search.getValue().trim().toLowerCase();
		return this.options.filter(({ label }) => label.toLowerCase().includes(query));
	}

	handleInput(data: string) {
		if (matchesKey(data, TOGGLE_SHORTCUT) || matchesKey(data, Key.ctrl("c"))) {
			this.done(null);
			return;
		}
		if (matchesKey(data, Key.enter)) {
			this.done(Array.from(this.changes, ([option, enabled]) => ({ option, enabled })));
			return;
		}
		if (matchesKey(data, Key.escape)) {
			if (!this.search.getValue()) {
				this.done(null);
				return;
			}
			this.search.setValue("");
			this.cursor = 0;
		} else if (matchesKey(data, Key.up)) {
			this.cursor = Math.max(0, this.cursor - 1);
		} else if (matchesKey(data, Key.down)) {
			this.cursor = Math.max(0, Math.min(this.filtered().length - 1, this.cursor + 1));
		} else if (matchesKey(data, Key.space)) {
			const option = this.filtered()[this.cursor];
			if (option) {
				const enabled = !(this.changes.get(option) ?? option.enabled);
				if (enabled === option.enabled) this.changes.delete(option);
				else this.changes.set(option, enabled);
			}
		} else {
			const previous = this.search.getValue();
			this.search.handleInput(data);
			if (this.search.getValue() !== previous) this.cursor = 0;
		}
		this.redraw();
	}

	render(width: number): string[] {
		if (width < 1) return [""];
		const filtered = this.filtered();
		const maxRows = Math.max(1, this.maxRows());
		const start = Math.max(
			0,
			Math.min(this.cursor - Math.floor(maxRows / 2), filtered.length - maxRows),
		);
		const rows = filtered.slice(start, start + maxRows).map((option, index) => {
			const selected = start + index === this.cursor;
			const enabled = this.changes.get(option) ?? option.enabled;
			return this.theme.fg(
				selected ? "accent" : "text",
				`${selected ? ">" : " "} [${enabled ? "x" : " "}] ${option.label}`,
			);
		});
		return [
			this.theme.fg("accent", `Toggle Pi resources · ${this.changes.size} pending`),
			...this.search.render(width),
			...(rows.length ? rows : [this.theme.fg("muted", "No matching resources")]),
			this.theme.fg(
				"dim",
				"Type to filter · ↑/↓ move · Space toggle · Enter save · Esc clear/cancel",
			),
		].map((line) => truncateToWidth(line, width));
	}
}
