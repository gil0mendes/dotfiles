---@class lua.utils
local H = {}

---merges two tables
---@param t1 table
---@param t2 table
---@return table
function H._merge(t1, t2)
	for k, v in pairs(t2) do
		if type(v) == "table" then
			if type(t1[k] or false) == "table" then
				H._merge(t1[k] or {}, t2[k] or {})
			else
				t1[k] = v
			end
		else
			t1[k] = v
		end
	end
	return t1
end

---add spaces to each side of a string
---@param s string
---@param space number
---@param trailing_space number
---@return string
H._space = function(s, space, trailing_space)
	if type(s) ~= "string" or type(space) ~= "number" then
		return ""
	end
	local spaces = string.rep(" ", space)
	local trailing_spaces = spaces
	if trailing_space ~= nil then
		trailing_spaces = string.rep(" ", trailing_space)
	end
	return spaces .. s .. trailing_spaces
end

---get basename for dir/file, removing ft and path
---@param s string
---@return string?
---@return number?
H._basename = function(s)
	if type(s) ~= "string" then
		return nil
	end
	return s:gsub("(.*[/\\])(.*)%.(.*)", "%2")
end

---cleanup the process name
---@return string
function H.get_clean_process_name(pane)
	local name = pane:get_foreground_process_name() or "Shell"
	name = name:match("([^/]+)$") or name
	return name:gsub("^%x+%-", "") -- Remove Nix hash
end

return H
