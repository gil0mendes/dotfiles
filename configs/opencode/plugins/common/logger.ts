import { appendFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";

type LogLevel = "DEBUG" | "INFO" | "WARN" | "ERROR";

export class Logger {
	constructor(private readonly filePath: string) {
		mkdirSync(dirname(filePath), { recursive: true });
	}

	private log(level: LogLevel, message: string): void {
		const timestamp = new Date().toISOString();
		const entry = `[${timestamp}] [${level}] ${message}\n`;

		appendFileSync(this.filePath, entry, "utf8");
	}

	debug(message: string): void {
		this.log("DEBUG", message);
	}

	info(message: string): void {
		this.log("INFO", message);
	}

	warn(message: string): void {
		this.log("WARN", message);
	}

	error(message: string): void {
		this.log("ERROR", message);
	}
}
