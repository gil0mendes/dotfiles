local wezterm = require("wezterm")
local window_state = require("lua.resurrect.window_state")
local utils = require("lua.resurrect.utils")

local M = {}

function M.get_workspace_state(workspace)
	local window_states = {}
	for _, window in ipairs(wezterm.mux.all_windows()) do
		if window:get_workspace() == workspace then
			table.insert(window_states, window_state.get_window_state(window))
		end
	end
	return { workspace = workspace, window_states = window_states }
end

function M.is_valid(state)
	if
		type(state) ~= "table"
		or type(state.workspace) ~= "string"
		or state.workspace == ""
		or type(state.window_states) ~= "table"
		or #state.window_states == 0
	then
		return false
	end
	for _, saved_window in ipairs(state.window_states) do
		if not window_state.is_valid(saved_window) then
			return false
		end
	end
	return true
end

function M.restore_workspace(state, opts)
	if not M.is_valid(state) then
		utils.error("refusing to restore malformed workspace state")
		return false
	end

	for _, saved_window in ipairs(state.window_states) do
		local spawn_options = {
			width = saved_window.size.cols,
			height = saved_window.size.rows,
			cwd = saved_window.tabs[1].pane_tree.cwd,
		}
		local domain = saved_window.tabs[1].pane_tree.domain
		if domain ~= nil then
			spawn_options.domain = { DomainName = domain }
		end
		local tab, pane, window = wezterm.mux.spawn_window(spawn_options)
		local window_options = {}
		for key, value in pairs(opts or {}) do
			window_options[key] = value
		end
		window_options.tab = tab
		window_options.pane = pane
		if not window_state.restore_window(window, saved_window, window_options) then
			return false
		end
	end
	return true
end

return M
