local wezterm = require("wezterm")
local file_io = require("lua.resurrect.file_io")
local tab_state = require("lua.resurrect.tab_state")
local utils = require("lua.resurrect.utils")
local window_state = require("lua.resurrect.window_state")
local workspace_state = require("lua.resurrect.workspace_state")

local M = {}

local validators = {
	tab = tab_state.is_valid,
	window = window_state.is_valid,
	workspace = workspace_state.is_valid,
}

local function save_state(id, kind, state)
	local validator = validators[kind]
	local path = file_io.path_for(kind, id)
	if validator == nil or path == nil or not validator(state) then
		utils.error("refusing to save malformed " .. kind .. " state")
		return false
	end
	local ok, encoded = pcall(wezterm.json_encode, state)
	if not ok or type(encoded) ~= "string" then
		utils.error("could not encode " .. kind .. " state")
		return false
	end
	return file_io.write(path, encoded)
end

function M.load_state(id, kind)
	local validator = validators[kind]
	local path = file_io.path_for(kind, id)
	if validator == nil or path == nil then
		utils.error("refusing to load state with invalid identifier or type")
		return nil
	end
	local contents = file_io.read(path)
	if contents == nil then
		return nil
	end
	local ok, state = pcall(wezterm.json_parse, contents)
	if not ok or not validator(state) then
		utils.error("refusing malformed " .. kind .. " state")
		return nil
	end
	return state
end

local function save_cycle(workspace, options)
	local successful = true
	if options.save_workspaces then
		successful = save_state(workspace, "workspace", workspace_state.get_workspace_state(workspace)) and successful
	end
	for _, mux_window in ipairs(wezterm.mux.all_windows()) do
		if mux_window:get_workspace() == workspace then
			local window_title = mux_window:get_title()
			if options.save_windows and window_title ~= "" then
				successful = save_state(window_title, "window", window_state.get_window_state(mux_window)) and successful
			end
			if options.save_tabs then
				for _, tab_info in ipairs(mux_window:tabs_with_info()) do
					local tab_title = tab_info.tab:get_title()
					if tab_title ~= "" then
						successful = save_state(tab_title, "tab", tab_state.get_tab_state(tab_info.tab, tab_info)) and successful
					end
				end
			end
		end
	end
	return successful
end

function M.periodic_save(options)
	options = options or {}
	local interval_seconds = options.interval_seconds or 900
	if type(interval_seconds) ~= "number" or interval_seconds <= 0 then
		utils.error("periodic save interval must be positive")
		return
	end

	wezterm.on("update-status", function(window)
		local now = os.time()
		local last_save = wezterm.GLOBAL.resurrect_last_save or 0
		if now - last_save < interval_seconds then
			return
		end
		local workspace = window:active_workspace()
		if type(workspace) ~= "string" or workspace == "" then
			utils.error("refusing to save an unnamed workspace")
			return
		end
		if not save_cycle(workspace, options) then
			return
		end
		wezterm.GLOBAL.resurrect_last_save = now
	end)
end

return M
