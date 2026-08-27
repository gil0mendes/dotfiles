local wezterm = require("wezterm")
local file_io = require("lua.resurrect.file_io")
local utils = require("lua.resurrect.utils")

local M = {}

local state_kinds = { "workspace", "window", "tab" }

local function choices()
	local result = {}
	for _, kind in ipairs(state_kinds) do
		local pattern = file_io.root .. "/" .. kind .. "/*.json"
		for _, path in ipairs(wezterm.glob(pattern)) do
			local encoded_id = path:match("/([^/]+)%.json$")
			local id = file_io.decode_identifier(encoded_id)
			if id ~= nil then
				table.insert(result, { id = kind .. "/" .. id, label = kind .. ": " .. id })
			end
		end
	end
	return result
end

function M.fuzzy_load(window, pane, on_select)
	if type(on_select) ~= "function" then
		utils.error("fuzzy loader requires a selection callback")
		return
	end

	local saved_states = choices()
	if #saved_states == 0 then
		utils.error("no saved states found")
		return
	end

	window:perform_action(
		wezterm.action.InputSelector({
			title = "Restore saved state",
			fuzzy = true,
			choices = saved_states,
			action = wezterm.action_callback(function(_, _, id)
				if id ~= nil then
					on_select(id)
				end
			end),
		}),
		pane
	)
end

return M
