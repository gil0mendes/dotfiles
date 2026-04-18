import { homedir, platform } from "node:os";
import { join } from "node:path";

const DEBOUNCE_MS = 1000;

export const NotificationPlugin = async ({ $, client }) => {
  const soundPath = join(homedir(), ".config/opencode/sounds/new-alert.mp3");
  const currentPlatform = platform();
  const lastSoundTime = {};

  const isDebounced = (eventType) => {
    const now = Date.now();
    const lastTime = lastSoundTime[eventType] ?? 0;

    if (now - lastTime < DEBOUNCE_MS) {
      return true;
    }

    lastSoundTime[eventType] = now;
    return false;
  };

  const tryPlayCommand = async (command) => {
    try {
      await command.quiet();
      return true;
    } catch {
      return false;
    }
  };

  const playNotificationSound = async () => {
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

  const notifyUser = async (eventType) => {
    if (isDebounced(eventType)) {
      return;
    }

    await playNotificationSound();
  };

  // Check if a session is a main (non-subagent) session
  const isMainSession = async (sessionID) => {
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
      if (event.type === "permission.asked") {
        await notifyUser(event.type);
      }
    },
  };
};
