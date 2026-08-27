local wezterm = require("wezterm")

local M = {}

local max_scrollback_lines = 3500

local function is_nonempty_argv(argv)
	if type(argv) ~= "table" or #argv == 0 then
		return false
	end
	for index, argument in pairs(argv) do
		if type(index) ~= "number" or index < 1 or index > #argv or index % 1 ~= 0 or type(argument) ~= "string" then
			return false
		end
	end
	return true
end

local function compare_by_coordinates(left, right)
	if left.left == right.left then
		return left.top < right.top
	end
	return left.left < right.left
end

local function capture_pane(pane_info)
	local pane = pane_info.pane
	local cwd = pane:get_current_working_dir()
	local state = {
		left = pane_info.left,
		top = pane_info.top,
		width = pane_info.width,
		height = pane_info.height,
		cwd = cwd and cwd.file_path or nil,
		domain = pane:get_domain_name(),
		is_active = pane_info.is_active == true,
		is_zoomed = pane_info.is_zoomed == true,
		is_alt_screen_active = false,
	}

	if state.domain ~= "local" then
		return state
	end

	state.is_alt_screen_active = pane:is_alt_screen_active() == true
	if state.is_alt_screen_active then
		local process = pane:get_foreground_process_info()
		if type(process) == "table" and is_nonempty_argv(process.argv) then
			state.process = { argv = process.argv }
		end
	else
		state.text = pane:get_lines_as_escapes(max_scrollback_lines)
	end
	return state
end

local function connected_pane(panes, visited, predicate)
	for _, pane in ipairs(panes) do
		if not visited[pane] and predicate(pane) then
			return pane
		end
	end
	return nil
end

local function build_tree(root, panes, visited)
	if root == nil then
		return nil
	end
	visited[root] = true

	local right = connected_pane(panes, visited, function(pane)
		return root.top == pane.top and root.left + root.width + 1 == pane.left
	end)
	if right ~= nil then
		root.right = build_tree(right, panes, visited)
	end

	local bottom = connected_pane(panes, visited, function(pane)
		return root.left == pane.left and root.top + root.height + 1 == pane.top
	end)
	if bottom ~= nil then
		root.bottom = build_tree(bottom, panes, visited)
	end

	return root
end

local function count_nodes(tree)
	if tree == nil then
		return 0
	end
	return 1 + count_nodes(tree.right) + count_nodes(tree.bottom)
end

function M.capture(tab)
	local panes = {}
	for _, pane_info in ipairs(tab:panes_with_info()) do
		table.insert(panes, capture_pane(pane_info))
	end
	local pane_count = #panes
	if pane_count == 0 then
		return nil
	end

	table.sort(panes, compare_by_coordinates)
	local root = panes[1]
	root = build_tree(root, panes, {})
	if count_nodes(root) ~= pane_count then
		return nil
	end
	return root
end

local function is_valid_pane(state, seen, counts)
	if type(state) ~= "table" or seen[state] then
		return false
	end
	seen[state] = true
	if
		type(state.left) ~= "number"
		or type(state.top) ~= "number"
		or type(state.width) ~= "number"
		or type(state.height) ~= "number"
		or state.width <= 0
		or state.height <= 0
		or type(state.domain) ~= "string"
		or state.domain == ""
		or type(state.is_active) ~= "boolean"
		or type(state.is_zoomed) ~= "boolean"
		or type(state.is_alt_screen_active) ~= "boolean"
		or (state.cwd ~= nil and type(state.cwd) ~= "string")
	then
		return false
	end

	if state.domain == "local" then
		if state.is_alt_screen_active then
			if type(state.process) ~= "table" or not is_nonempty_argv(state.process.argv) then
				return false
			end
		elseif type(state.text) ~= "string" then
			return false
		end
	elseif state.is_alt_screen_active or state.text ~= nil or state.process ~= nil then
		return false
	end

	counts.active = counts.active + (state.is_active and 1 or 0)
	counts.zoomed = counts.zoomed + (state.is_zoomed and 1 or 0)
	counts.active_zoomed = counts.active_zoomed + (state.is_active and state.is_zoomed and 1 or 0)
	if state.right ~= nil then
		if state.right.left ~= state.left + state.width + 1 or state.right.top ~= state.top then
			return false
		end
		if not is_valid_pane(state.right, seen, counts) then
			return false
		end
	end
	if state.bottom ~= nil then
		if state.bottom.left ~= state.left or state.bottom.top ~= state.top + state.height + 1 then
			return false
		end
		if not is_valid_pane(state.bottom, seen, counts) then
			return false
		end
	end
	return true
end

function M.is_valid(tree)
	local counts = { active = 0, zoomed = 0, active_zoomed = 0 }
	if not is_valid_pane(tree, {}, counts) then
		return false
	end
	return counts.active == 1 and counts.zoomed <= 1 and counts.zoomed == counts.active_zoomed
end

return M
