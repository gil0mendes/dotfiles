import { spawn } from "node:child_process";
import { homedir, platform } from "node:os";
import { join } from "node:path";
import { Plugin } from "@opencode/plugin";

const debounceMs = 1_000;

function play(command: string, soundPath: string): Promise<void> {
	return new Promise((resolve, reject) => {
		const process = spawn(command, [soundPath], { stdio: "ignore" });
		process.once("error", reject);
		process.once("exit", (code) => {
			if (code === 0) resolve();
			else reject(new Error(`${command} exited with ${code ?? "unknown"}`));
		});
	});
}

export default Plugin.define({
	id: "notification",
	setup(ctx) {
		const soundDirectory = join(homedir(), ".config/opencode/sounds");
		const lastSoundAt = new Map<string, number>();
		const controller = new AbortController();

		const notify = async (eventType: string) => {
			const now = Date.now();
			const previous = lastSoundAt.get(eventType) ?? 0;
			if (now - previous < debounceMs) return;
			lastSoundAt.set(eventType, now);

			const sound = eventType === "permission.asked" ? "ding.mp3" : "new-alert.mp3";
			const soundPath = join(soundDirectory, sound);
			if (platform() === "darwin") {
				await play("afplay", soundPath);
				return;
			}
			for (const command of ["paplay", "pw-play", "mpv"]) {
				try {
					await play(command, soundPath);
					return;
				} catch {
					// Try the next available player.
				}
			}
		};

		void (async () => {
			for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
				if (event.type === "permission.asked") {
					await notify(event.type);
					continue;
				}
				if (event.type !== "session.idle") continue;
				const session = await ctx.session.get({ sessionID: event.data.sessionID });
				if (!session.parentID) await notify(event.type);
			}
		})().catch((error: unknown) => {
			if (!controller.signal.aborted) console.warn("[notification]", error);
		});

		return () => controller.abort();
	},
});
