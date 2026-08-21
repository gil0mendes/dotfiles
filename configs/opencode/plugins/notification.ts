import type { Plugin } from "@opencode-ai/plugin";
import type { EventSessionIdle } from "@opencode-ai/sdk";
import { homedir, platform } from "node:os";
import { join } from "node:path";

const DEBOUNCE_MS = 1000;

const SOUNDS = {
	finished: "new-alert.mp3",
	alert: "ding.mp3",
};

const SOUNDS_BY_EVENT: Record<string, keyof typeof SOUNDS> = {
	"permission.asked": "alert",
	"session.idle": "finished"
};

export const NotificationPlugin: Plugin = async ({ $, client }) => {
	const soundDirectory = join(homedir(), ".config/opencode/sounds");
  const currentPlatform = platform();
  const lastSoundTime: Record<string, number> = {};

  const isDebounced = (eventType: string) => {
    const now = Date.now();
    const lastTime = lastSoundTime[eventType] ?? 0;

    if (now - lastTime < DEBOUNCE_MS) {
      return true;
    }

    lastSoundTime[eventType] = now;
    return false;
  };

  const tryPlayCommand = async (command: BunShell) => {
    try {
      await command.quiet();
      return true;
    } catch {
      return false;
    }
  };

	const playNotificationSound = async (sound: keyof typeof SOUNDS) => {
		const soundPath = join(soundDirectory, SOUNDS[sound]);

    if (currentPlatform === "darwin") {
      return $`afplay ${soundPath}`.quiet();
    }

    if (currentPlatform === "linux") {
      if (await tryPlayCommand($`paplay ${soundPath}`)) {
        return;
      }

      if (await tryPlayCommand($`pw-play ${soundPath}`)) {
        return;
      }

      return $`mpv --no-video --really-quiet ${soundPath}`.quiet();
    }
  };

  const notifyUser = async (eventType: string) => {
    if (isDebounced(eventType)) {
      return;
		}

		const soundToPlay = SOUNDS_BY_EVENT[eventType] ?? "finished";
    await playNotificationSound(soundToPlay);
  };

  // Check if a session is a main (non-subagent) session
  const isMainSession = async (
    sessionID: EventSessionIdle["properties"]["sessionID"],
  ) => {
    try {
      const result = await client.session.get({ path: { id: sessionID } });
      const session = result.data ?? result;
      return !session.parentID;
    } catch {
      // If we can't fetch the session, assume it's main to avoid missing notifications
      return true;
    }
  };

  return {
    event: async ({ event }) => {
      // Only notify for main session events, not background subagents
      if (event.type === "session.idle") {
        const sessionID = event.properties.sessionID;
        if (await isMainSession(sessionID)) {
          await notifyUser(event.type);
        }
			}

			// Permission prompt created
      // @ts-expect-error this is a work in progress
      if (event.type === "permission.asked") {
        await notifyUser(event.type);
      }
    },
  };
};
