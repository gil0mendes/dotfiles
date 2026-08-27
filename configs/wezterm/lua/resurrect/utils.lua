local wezterm = require("wezterm")

local M = {}

function M.error(message)
	wezterm.emit("resurrect.error", "resurrect: " .. message)
end

function M.is_array_of_strings(value)
	if type(value) ~= "table" then
		return false
	end

	for index, item in ipairs(value) do
		if type(index) ~= "number" or type(item) ~= "string" then
			return false
		end
	end

	return next(value) == nil or #value > 0
end

function M.current_working_dir(pane)
	local cwd = pane:get_current_working_dir()
	if cwd == nil then
		return nil
	end

	return cwd.file_path
end

function M.process_argv(pane)
	local process = pane:get_foreground_process_info()
	if type(process) ~= "table" or not M.is_array_of_strings(process.argv) then
		return nil
	end

	return process.argv
end

return M
