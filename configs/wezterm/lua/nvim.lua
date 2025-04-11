local wez = require("wezterm")
local smart_splits = wez.plugin.require("https://github.com/mrjones2014/smart-splits.nvim")

local M = {}

local function find_vim_pane(tab)
	for _, pane in ipairs(tab:panes()) do
		if smart_splits.is_vim(pane) then
			return pane
		end
	end
end

M.apply_to_config = function(config)
	smart_splits.apply_to_config(config)
end

return M
