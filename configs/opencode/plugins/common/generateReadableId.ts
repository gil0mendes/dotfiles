import {
	adjectives,
	animals,
	colors,
	uniqueNamesGenerator,
} from "unique-names-generator";

export function generateReadableId(): string {
	return uniqueNamesGenerator({
		dictionaries: [adjectives, colors, animals],
		separator: "-",
		length: 3,
		style: "lowerCase",
	});
}
