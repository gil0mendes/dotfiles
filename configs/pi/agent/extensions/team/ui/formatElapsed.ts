export const formatElapsed = (startedAt: number, endedAt: number): string => {
	const elapsedSeconds = Math.max(0, Math.floor((endedAt - startedAt) / 1000));
	const minutes = Math.floor(elapsedSeconds / 60);
	const seconds = elapsedSeconds % 60;
	if (minutes === 0) return `${seconds}s`;
	return `${minutes}m${String(seconds).padStart(2, "0")}s`;
};
