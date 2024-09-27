local wez = require("wezterm")
local act = wez.action
local callback = wez.action_callback

local mod = {
	c = "CTRL",
	s = "SHIFT",
	a = "ALT",
	l = "LEADER",
}

local keybind = function(mods, key, action)
	return { mods = table.concat(mods, "|"), key = key, action = action }
end

local M = {}

local leader = { mods = mod.c, key = "a", timeout_miliseconds = 1000 }

local keys = function() end

M.apply_to_config = function(config)
	config.treat_left_ctrlalt_as_altgr = true
	config.leader = leader
	config.keys = keys()
end

return M
