local wez = require("wezterm")
local utilities = require("lua.utilities")

local utils = require("lua.utilities")

local options = {
	position = "bottom",
	max_width = 32,
	padding = {
		left = 1,
		right = 1,
	},
	separator = {
		space = 1,
		left_icon = wez.nerdfonts.fa_long_arrow_right,
		right_icon = wez.nerdfonts.fa_long_arrow_left,
		field_icon = wez.nerdfonts.indent_line,
	},
	modules = {
		tabs = {
			active_tab_fg = 4,
			inactive_tab_fg = 6,
		},
		pane = {
			icon = wez.nerdfonts.cod_multiple_windows,
			color = 7,
		},
		clock = {
			icon = wez.nerdfonts.md_calendar_clock,
			color = 5,
		},
	},
}

---get tab title
---@param tab_info tabs.tabinfo
---@return string
local tab_get_title = function(tab_info)
	local title = tab_info.tab_title
	-- if the tg title is expplicitly set, ttake that
	if title and #title > 0 then
		return title
	end

	-- otherwise, use the title from the active pane in that tab
	return utilities._basename(tab_info.active_pane.title)
end

local M = {}

M.apply_to_config = function(config)
	local scheme = wez.color.get_builtin_schemes()[config.color_scheme]
	if scheme == nil then
		scheme = wez.color.get_default_colors()
	end

	local default_colors = {
		tab_bar = {
			background = "transparent",
			active_tab = {
				bg_color = "transparent",
				fg_color = scheme.ansi[options.modules.tabs.active_tab_fg],
			},
			inactive_tab = {
				bg_color = "transparent",
				fg_color = scheme.ansi[options.modules.tabs.inactive_tab_fg],
			},
		},
	}

	config.colors = utils._merge(default_colors, scheme)

	-- make us own this settings
	config.tab_bar_at_bottom = true
	config.use_fancy_tab_bar = false
	config.show_new_tab_button_in_tab_bar = false
	config.tab_max_width = options.max_width
end

wez.on("format-tab-title", function(tab, _, _, conf, _, _)
	local palette = conf.resolved_palette

	local index = tab.tab_index + 1
	local offset = #tostring(index) + #options.separator.left_icon + (2 * options.separator.space) + 2
	local title = index
		.. utilities._space(options.separator.left_icon, options.separator.space, nil)
		.. tab_get_title(tab)

	local width = conf.tab_max_width - offset
	if #title > conf.tab_max_width then
		title = wez.truncate_right(title, width) .. "…"
	end

	local bg = palette.tab_bar.inactive_tab.bg_color
	local fg = palette.tab_bar.inactive_tab.fg_color
	if tab.is_active then
		fg = palette.tab_bar.active_tab.fg_color
		bg = palette.tab_bar.active_tab.bg_color
	end

	return {
		{ Background = { Color = bg } },
		{ Foreground = { Color = fg } },
		{ Text = utilities._space(title, 0, 2) },
	}
end)

wez.on("update-status", function(window, pane)
	print("HERE")
	local present, conf = pcall(window.effective_config, window)

	if not present then
		return
	end

	local palette = conf.resolved_palette

	-- left status
	local left_cells = {
		{ Background = { Color = palette.tab_bar.background } },
	}

	table.insert(left_cells, { Text = string.rep(" ", options.padding.left) })

	local process = utilities.get_clean_process_name(pane)
	if process then
		table.insert(left_cells, { Foreground = { Color = palette.ansi[options.modules.pane.color] } })
		table.insert(left_cells, {
			Text = options.modules.pane.icon .. utilities._space(process, options.separator.space),
		})
	end

	window:set_left_status(wez.format(left_cells))

	-- right status
	local right_cells = {
		{ Background = { Color = palette.tab_bar.background } },
	}

	local callbacks = {
		{
			name = "clock",
			func = function()
				return wez.time.now():format("%H:%M")
			end,
		},
	}

	for _, callback in ipairs(callbacks) do
		local name = callback.name
		local func = callback.func

		local text = func()
		if #text > 0 then
			table.insert(right_cells, { Foreground = { Color = palette.ansi[options.modules[name].color] } })
			table.insert(right_cells, { Text = text })
			table.insert(right_cells, { Foreground = { Color = palette.brights[1] } })
			table.insert(right_cells, {
				Text = utilities._space(options.separator.right_icon, options.separator.space, nil)
					.. options.modules[name].icon,
			})
			table.insert(
				right_cells,
				{ Text = utilities._space(options.separator.field_icon, options.separator.space, nil) }
			)
		end
	end

	-- remove trailing separator
	table.remove(right_cells, #right_cells)
	table.insert(right_cells, { Text = string.rep(" ", options.padding.right) })

	window:set_right_status(wez.format(right_cells))
end)

return M
