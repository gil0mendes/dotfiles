local wezterm = require("wezterm")
local utils = require("lua.resurrect.utils")

local M = {}

local state_home = os.getenv("XDG_STATE_HOME")
if type(state_home) ~= "string" or state_home:sub(1, 1) ~= "/" or state_home == "/" then
	state_home = wezterm.home_dir .. "/.local/state"
end

M.root = state_home .. "/wezterm/resurrect"

local function run(argv, description)
	local ok, _, stderr = wezterm.run_child_process(argv)
	if ok then
		return true
	end

	utils.error("could not " .. description .. (stderr ~= "" and ": " .. stderr or ""))
	return false
end

function M.ensure_directory(path)
	if not run({ "mkdir", "-p", "-m", "700", M.root }, "create state directory") then
		return false
	end
	if not run({ "chmod", "700", M.root }, "secure state directory") then
		return false
	end
	if path == M.root then
		return true
	end
	if not run({ "mkdir", "-p", "-m", "700", path }, "create state directory") then
		return false
	end
	return run({ "chmod", "700", path }, "secure state directory")
end

local valid_kinds = { workspace = true, window = true, tab = true }

function M.encode_identifier(id)
	if type(id) ~= "string" or id == "" or id:find("%z") then
		return nil
	end
	return (id:gsub("[^%w%-%._]", function(character)
		return string.format("%%%02X", string.byte(character))
	end))
end

function M.decode_identifier(encoded)
	if type(encoded) ~= "string" or encoded == "" then
		return nil
	end
	local offset = 1
	while true do
		local percent = encoded:find("%", offset, true)
		if percent == nil then
			break
		end
		if not encoded:sub(percent + 1, percent + 2):match("^%x%x$") then
			return nil
		end
		offset = percent + 3
	end
	local decoded = encoded:gsub("%%(%x%x)", function(hex)
		return string.char(tonumber(hex, 16))
	end)
	if decoded:find("%z") then
		return nil
	end
	return decoded
end

function M.path_for(kind, id)
	if valid_kinds[kind] ~= true then
		return nil
	end
	local encoded = M.encode_identifier(id)
	if encoded == nil then
		return nil
	end
	return M.root .. "/" .. kind .. "/" .. encoded .. ".json"
end

function M.write(path, contents)
	local directory = path:match("^(.*)/[^/]+$")
	if directory == nil or not M.ensure_directory(directory) then
		return false
	end

	local temporary_path = path .. ".tmp-" .. tostring(os.time()) .. "-" .. tostring(math.random(1000000))
	local file, open_error = io.open(temporary_path, "w")
	if file == nil then
		utils.error("could not open temporary state file: " .. tostring(open_error))
		return false
	end

	local wrote, write_error = file:write(contents)
	local flushed, flush_error = file:flush()
	local closed, close_error = file:close()
	if not wrote or not flushed or not closed then
		os.remove(temporary_path)
		utils.error("could not write state file: " .. tostring(write_error or flush_error or close_error))
		return false
	end
	if not run({ "chmod", "600", temporary_path }, "secure state file") then
		os.remove(temporary_path)
		return false
	end
	if not os.rename(temporary_path, path) then
		os.remove(temporary_path)
		utils.error("could not replace state file")
		return false
	end

	return true
end

function M.read(path)
	local file, open_error = io.open(path, "r")
	if file == nil then
		utils.error("could not open state file: " .. tostring(open_error))
		return nil
	end

	local contents, read_error = file:read("*a")
	local closed, close_error = file:close()
	if contents == nil or not closed then
		utils.error("could not read state file: " .. tostring(read_error or close_error))
		return nil
	end

	return contents
end

return M
