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

	-- fonts
	config.font_size = 15
	config.font = wez.font("Comic Code Ligatures")
	config.bold_brightens_ansi_colors = true
	config.font_rules = {
		{
			italic = true,
			intensity = "Half",
			font = wez.font("Comic Code Ligatures", { italic = true }),
		},
	}
	config.line_height = 1.2
	config.cell_width = 1.0

	-- cursor
	config.underline_thickness = 1
	config.cursor_thickness = 1
	config.underline_position = -4
	config.default_cursor_style = "BlinkingBar"
	config.force_reverse_video_cursor = true
	config.cursor_blink_ease_in = "Constant"
	config.cursor_blink_ease_out = "Constant"
end

return M
