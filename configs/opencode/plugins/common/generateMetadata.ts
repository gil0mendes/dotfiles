type GeneratedMetadata = {
	title: string;
	description: string;
};

/**
 * Build deterministic metadata without creating a second session.
 * V2 no longer exposes the legacy small_model configuration field.
 */
export async function generateMetadata(
	_resultContent: string,
	debugLog: (message: string) => Promise<void>,
): Promise<GeneratedMetadata> {
	const firstLine =
		_resultContent.split("\n").find((line) => line.trim().length > 0) ??
		"Delegation result";
	const title = `${firstLine.slice(0, 30).trim()}${firstLine.length > 30 ? "..." : ""}`;
	const description = `${_resultContent.slice(0, 150).trim()}${
		_resultContent.length > 150 ? "..." : ""
	}`;

	await debugLog(`generateMetadata: Generated title="${title}"`);
	return { title, description };
}
