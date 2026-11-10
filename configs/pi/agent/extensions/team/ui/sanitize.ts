// Terminal strings are untrusted even after Markdown rendering: links emit OSC sequences.
/* oxlint-disable no-control-regex -- Terminal controls are deliberately matched here. */
export function sanitizeTerminalText(value: string, allowSgr = false): string {
	return value
		.replace(/(?:\x1b[\]PX^_]|[\x90\x9d\x98\x9e\x9f])[\s\S]*?(?:\x07|\x1b\\|\x9c|$)/g, "")
		.replace(
			/(?:\x1b\[|\x9b)[0-?]*[ -/]*[@-~]?|\x1b(?:[ -/]*[0-~])?|[\x00-\x09\x0b-\x1f\x7f-\x9f\u061c\u200e\u200f\u202a-\u202e\u2066-\u2069]/g,
			(sequence) => (allowSgr && /^\x1b\[[0-9;:]*m$/.test(sequence) ? sequence : ""),
		);
}
/* oxlint-enable no-control-regex */
