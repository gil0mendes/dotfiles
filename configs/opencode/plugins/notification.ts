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
		const activeSoundTypes = new Set<string>();
		const controller = new AbortController();

		const notify = (eventType: string): Promise<void> | undefined => {
			const now = Date.now();
			const previous = lastSoundAt.get(eventType) ?? 0;
			if (activeSoundTypes.has(eventType) || now - previous < debounceMs) return;

			activeSoundTypes.add(eventType);
			lastSoundAt.set(eventType, now);

			return (async () => {
				try {
					const sound = eventType === "permission.asked" ? "ding.mp3" : "new-alert.mp3";
					const soundPath = join(soundDirectory, sound);
					if (platform() === "darwin") {
						await play("afplay", soundPath);
						return;
					}
					let finalPlaybackFailure: Error | undefined;
					for (const command of ["paplay", "pw-play", "mpv"]) {
						try {
							await play(command, soundPath);
							return;
						} catch (error: unknown) {
							if (error instanceof Error) finalPlaybackFailure = error;
							// Try the next available player.
						}
					}
					throw (
						finalPlaybackFailure ??
						new Error("Unable to play notification sound: every fallback player failed.")
					);
				} finally {
					activeSoundTypes.delete(eventType);
				}
			})();
		};

		const handlePlaybackFailure = (playback: Promise<void> | undefined) => {
			if (!playback) return;
			void playback.catch((error: unknown) => {
				if (!controller.signal.aborted) console.warn("[notification]", error);
			});
		};

		void (async () => {
			for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
				if (event.type === "permission.asked") {
					handlePlaybackFailure(notify(event.type));
					continue;
				}
				if (event.type !== "session.idle") continue;
				const session = await ctx.session.get({ sessionID: event.data.sessionID });
				if (!session.parentID) handlePlaybackFailure(notify(event.type));
			}
		})().catch((error: unknown) => {
			if (!controller.signal.aborted) console.warn("[notification]", error);
		});

		return () => controller.abort();
	},
});
