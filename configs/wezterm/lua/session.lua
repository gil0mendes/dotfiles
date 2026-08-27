local wez = require("wezterm")
local resurrect = require("lua.resurrect")

local M = {}

M.apply_to_config = function(config)
	resurrect.state_manager.periodic_save({
		interval_seconds = 15 * 60,
		save_workspaces = true,
		save_windows = true,
		save_tabs = true,
	})

	wez.on("resurrect.error", function(err)
		wez.log_error(err)
		local gui_windows = wez.gui.gui_windows()
		if gui_windows[1] ~= nil then
			gui_windows[1]:toast_notification("resurrect", err, nil, 3000)
		end
	end)
end

return M
