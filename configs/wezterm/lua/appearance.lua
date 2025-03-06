local wez = require("wezterm")

local M = {}

M.apply_to_config = function(config)
	config.color_scheme = "rose-pine-moon"
	local scheme = wez.color.get_builtin_schemes()["rose-pine-moon"]
	config.colors = {
		split = scheme.ansi[2],
	}
	config.window_background_opacity = 0.96
	config.inactive_pane_hsb = { brightness = 0.9 }
	config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
	config.window_decorations = "RESIZE|TITLE|MACOS_FORCE_ENABLE_SHADOW"
end

return M
