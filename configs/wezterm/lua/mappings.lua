local wez = require("wezterm")
local act = wez.action
local callback = wez.action_callback
local resurrect = wez.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

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

local leader = { mods = mod.c, key = ";", timeout_miliseconds = 1000 }

local keys = function()
	local keys = {
		-- pane and tabs
		keybind({ mod.l }, "s", act.SplitVertical({ domain = "CurrentPaneDomain" })),
		keybind({ mod.l }, "v", act.SplitHorizontal({ domain = "CurrentPaneDomain" })),
		keybind({ mod.l }, "z", act.TogglePaneZoomState),
		keybind({ mod.l }, "x", act.CloseCurrentPane({ confirm = true })),
		keybind({ mod.l }, "c", act.SpawnTab("CurrentPaneDomain")),
		--- move between panes
		keybind({ mod.l }, "h", act.ActivatePaneDirection("Left")),
		keybind({ mod.l }, "j", act.ActivatePaneDirection("Down")),
		keybind({ mod.l }, "k", act.ActivatePaneDirection("Up")),
		keybind({ mod.l }, "l", act.ActivatePaneDirection("Right")),
		--- rename tab
		keybind(
			{ mod.l },
			"e",
			act.PromptInputLine({
				description = wez.format({
					{ Attribute = { Intensity = "Bold" } },
					{ Foreground = { AnsiColor = "Fuchsia" } },
					{ Text = "Renaming Tab title....:" },
				}),
				action = callback(function(win, _, line)
					if line == "" then
						return
					end
					win:active_tab():set_title(line)
				end),
			})
		),

		-- workspaces
		keybind({ mod.l }, "w", act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" })),

		-- copy and paste
		keybind({ mod.c, mod.s }, "c", act.CopyTo("Clipboard")),
		keybind({ mod.c, mod.s }, "v", act.PasteFrom("Clipboard")),

		-- update all plugins
		keybind(
			{ mod.l },
			"u",
			callback(function(win)
				wez.plugin.update_all()
				win:toast_notification("wezterm", "plugins updated!", nil, 4000)
			end)
		),

		-- show the debug overlay
		keybind({ mod.l }, "d", wez.action.ShowDebugOverlay),
	}

	-- leader number navigates to the tab
	for i = 1, 9 do
		table.insert(keys, keybind({ mod.l }, tostring(i), act.ActivateTab(i - 1)))
	end

	-- restore session
	keybind(
		{ mod.l },
		"r",
		callback(function(win, pane)
			resurrect.fuzzy_loader.fuzzy_load(win, pane, function(id)
				local type = string.match(id, "^([^/]+)") -- match before '/'
				id = string.match(id, "([^/]+)$") -- match after '/'
				id = string.match(id, "(.+)%..+$") -- remove file extention
				local opts = {
					relative = true,
					restore_text = true,
					on_pane_restore = resurrect.tab_state.default_on_pane_restore,
				}
				if type == "workspace" then
					local state = resurrect.state_manager.load_state(id, "workspace")
					resurrect.workspace_state.restore_workspace(state, opts)
				elseif type == "window" then
					local state = resurrect.state_manager.load_state(id, "window")
					resurrect.window_state.restore_window(pane:window(), state, opts)
				elseif type == "tab" then
					local state = resurrect.state_manager.load_state(id, "tab")
					resurrect.tab_state.restore_tab(pane:tab(), state, opts)
				end
			end)
		end)
	)

	return keys
end

M.apply_to_config = function(config)
	config.treat_left_ctrlalt_as_altgr = true
	config.leader = leader
	config.keys = keys()
end

return M
