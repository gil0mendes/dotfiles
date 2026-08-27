local wezterm = require("wezterm")
local pane_tree = require("lua.resurrect.pane_tree")
local utils = require("lua.resurrect.utils")

local M = {}

function M.get_tab_state(tab, tab_info)
	local is_zoomed = false
	for _, pane_info in ipairs(tab:panes_with_info()) do
		if pane_info.is_zoomed then
			is_zoomed = true
			break
		end
	end
	return {
		title = tab:get_title(),
		is_active = tab_info.is_active == true,
		is_zoomed = is_zoomed,
		pane_tree = pane_tree.capture(tab),
	}
end

function M.is_valid(state)
	return type(state) == "table"
		and type(state.title) == "string"
		and type(state.is_active) == "boolean"
		and type(state.is_zoomed) == "boolean"
		and pane_tree.is_valid(state.pane_tree)
end

function M.default_on_pane_restore(pane, state)
	if state.is_alt_screen_active then
		pane:send_text(wezterm.shell_join_args(state.process.argv) .. "\r\n")
	elseif state.text ~= nil then
		local text = state.text:gsub("%s+$", "")
		if text ~= "" then
			pane:inject_output(text)
		end
	end
end

local function split_options(parent, child, direction, relative)
	local options = { direction = direction, cwd = child.cwd }
	if child.domain ~= nil then
		options.domain = { DomainName = child.domain }
	end
	if relative then
		if direction == "Right" then
			options.size = child.width / (parent.width + child.width)
		else
			options.size = child.height / (parent.height + child.height)
		end
	end
	return options
end

local function restore_pane(pane, state, opts, restored)
	if opts.on_pane_restore ~= nil then
		opts.on_pane_restore(pane, state)
	end
	if state.is_active then
		restored.active_pane = pane
	end

	local right_pane
	if state.right ~= nil then
		right_pane = pane:split(split_options(state, state.right, "Right", opts.relative))
	end
	local bottom_pane
	if state.bottom ~= nil then
		bottom_pane = pane:split(split_options(state, state.bottom, "Bottom", opts.relative))
	end
	if right_pane ~= nil then
		restore_pane(right_pane, state.right, opts, restored)
	end
	if bottom_pane ~= nil then
		restore_pane(bottom_pane, state.bottom, opts, restored)
	end
end

local function root_split_options(state)
	local options = { cwd = state.cwd }
	if state.domain ~= nil then
		options.domain = { DomainName = state.domain }
	end
	return options
end

function M.restore_tab(tab, state, opts)
	if not M.is_valid(state) then
		utils.error("refusing to restore malformed tab state")
		return false
	end

	opts = opts or {}
	local root_pane = opts.pane
	if root_pane == nil then
		root_pane = tab:active_pane():split(root_split_options(state.pane_tree))
	end
	local restored = {}
	restore_pane(root_pane, state.pane_tree, opts, restored)
	tab:set_title(state.title)
	restored.active_pane:activate()
	if state.is_zoomed then
		tab:set_zoomed(true)
	end
	return true
end

return M
