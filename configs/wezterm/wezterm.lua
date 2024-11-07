local wez = require("wezterm")

local appearance = require("lua.appearance")
local mappings = require("lua.mappings")
local bar = wez.plugin.require("https://github.com/adriankarlen/bar.wezterm")

local config = wez.config_builder()

-- reload wez automatically
config.automatically_reload_config = true

-- General configurations
config.font = wez.font("Comic Code Ligatures")
config.font_rules = {
	{
		italic = true,
		intensity = "Half",
		font = wez.font("Comic Code Ligatures", { italic = true }),
	},
}
config.font_size = 15
-- config.default_prog = { "fish" }
config.adjust_window_size_when_changing_font_size = false
config.audible_bell = "Disabled"
config.scrollback_lines = 3000
config.default_workspace = "main"
config.status_update_interval = 2000

-- Appearance
appearance.apply_to_config(config)

-- bar
bar.apply_to_config(config, {})

-- mappings
mappings.apply_to_config(config)

return config