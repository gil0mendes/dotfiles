local wez = require("wezterm")
local resurrect = wez.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

local M = {}

M.apply_to_config = function(config)
	resurrect.state_manager.periodic_save({
		interval_seconds = 15 * 60,
		save_workspaces = true,
		save_windows = true,
		save_tabs = true,
	})

	wez.on("resurrect.error", function(err)
		wez.log_error("ERROR!")
		wez.gui.gui_windows()[1]:toast_notification("resurrect", err, nil, 3000)
	end)
end

return M
