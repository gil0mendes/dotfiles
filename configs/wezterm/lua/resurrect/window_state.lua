local tab_state = require("lua.resurrect.tab_state")
local utils = require("lua.resurrect.utils")

local M = {}

function M.get_window_state(window)
	local tabs = {}
	local tab_infos = window:tabs_with_info()
	for _, tab_info in ipairs(tab_infos) do
		table.insert(tabs, tab_state.get_tab_state(tab_info.tab, tab_info))
	end
	return {
		title = window:get_title(),
		size = tab_infos[1].tab:get_size(),
		tabs = tabs,
	}
end

function M.is_valid(state)
	if
		type(state) ~= "table"
		or type(state.title) ~= "string"
		or type(state.size) ~= "table"
		or type(state.size.rows) ~= "number"
		or type(state.size.cols) ~= "number"
		or type(state.size.pixel_width) ~= "number"
		or type(state.size.pixel_height) ~= "number"
		or state.size.rows <= 0
		or state.size.cols <= 0
		or state.size.pixel_width <= 0
		or state.size.pixel_height <= 0
		or type(state.tabs) ~= "table"
		or #state.tabs == 0
	then
		return false
	end

	local active_tabs = 0
	for _, tab in ipairs(state.tabs) do
		if not tab_state.is_valid(tab) then
			return false
		end
		active_tabs = active_tabs + (tab.is_active and 1 or 0)
	end
	return active_tabs == 1
end

local function copy_options(opts)
	local copied = {}
	for key, value in pairs(opts or {}) do
		copied[key] = value
	end
	return copied
end

local function spawn_tab_options(saved_tab)
	local options = { cwd = saved_tab.pane_tree.cwd }
	if saved_tab.pane_tree.domain ~= nil then
		options.domain = { DomainName = saved_tab.pane_tree.domain }
	end
	return options
end

function M.restore_window(window, state, opts)
	if not M.is_valid(state) then
		utils.error("refusing to restore malformed window state")
		return false
	end

	window:set_title(state.title)
	local gui_window = window:gui_window()
	if gui_window ~= nil then
		gui_window:set_inner_size(state.size.pixel_width, state.size.pixel_height)
	end

	local active_tab
	for index, saved_tab in ipairs(state.tabs) do
		local tab
		local root_pane
		if index == 1 and opts ~= nil and opts.tab ~= nil then
			tab = opts.tab
			root_pane = opts.pane
		else
			tab, root_pane = window:spawn_tab(spawn_tab_options(saved_tab))
		end
		local tab_options = copy_options(opts)
		tab_options.pane = root_pane
		if not tab_state.restore_tab(tab, saved_tab, tab_options) then
			return false
		end
		if saved_tab.is_active then
			active_tab = tab
		end
	end

	active_tab:activate()
	return true
end

return M
