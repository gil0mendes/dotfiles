local wez = require("wezterm")

local appearance = require("lua.appearance")
local mappings = require("lua.mappings")
local bar = require("lua.bar")
local session = require("lua.session")

local config = wez.config_builder()

-- reload wez automatically
config.automatically_reload_config = true

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

-- window
config.adjust_window_size_when_changing_font_size = false
config.audible_bell = "Disabled"
config.scrollback_lines = 3000
config.default_workspace = "main"
config.status_update_interval = 2000

-- Appearance
appearance.apply_to_config(config)

-- bar
bar.apply_to_config(config)

-- session manager
session.apply_to_config(config)

-- mappings
mappings.apply_to_config(config)

return config
