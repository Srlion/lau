/*
    this is really shit code that needs to be changed
*/
Lau = {}

Lau.Modules = {
    {
        name = "colon_call",
        "__CALL__"
    },
    {
        name = "async",
        "__ASYNC__"
    },
    {
        name = "promise",
        "Promise"
    }
}

local function StripExtension( path )
	local i = path:match( ".+()%.%w+$" )
	if ( i ) then return path:sub( 1, i - 1 ) end
	return path
end

local function GetExtensionFromFilename( path )
	return path:match( "%.([^%.]+)$" )
end

local function GetPathFromFilename( path )
	return path:match( "^(.*[/\\])[^/\\]-$" ) or ""
end

if CLIENT then
    function Lau.RunFile(name)
        if isstring(name) then
            name = StripExtension(name) .. ".lua";
        end
        include(name)
    end

    for k, v in ipairs(Lau.Modules) do
        Lau.RunFile("lau/modules/" .. v.name .. ".lau")
    end

    timer.Simple(0, function()
        net.Receive("Lau.Refresh_Client_File", function()
            local file_name = net.ReadString()
            local code = net.ReadData(net.ReadUInt(32))
            code = util.Decompress(code)

            RunString(code, file_name)
        end)
    end)

    return
end

AddCSLuaFile()
require("gaceio")

-- print("\n\n\n\n\n\n\n\n\n\n\n-----------------------------------")

local function handle_error(fn, ...)
    local status, v = pcall(fn, ...)

    if (status == false) then
        return ErrorNoHalt("[ERROR] " .. v .. "\n")
    end

    return v
end

local lex = include("lau/lexer/mod.lua")
local parse = include("lau/parser/parser.lua")
local generate = include("lau/generator.lua")
AddCSLuaFile("lau/generator.lua")

-- this is cool
local function get_path(name)
    name = "lua/" .. name

    local in_addons = util.RelativePathToFull(name):gsub("\\", "/") == name
    if not in_addons then
        return name
    end

    local _, dirs = file.Find("addons/*", "GAME")
    for _, v in ipairs(dirs) do
        local v = "addons/" .. v .. "/" .. name
        local path = util.RelativePathToFull(v):gsub("\\", "/")
        if path ~= v then
            return v
        end
    end

    return false
end

local function ext_lua(name)
    return StripExtension(name) .. ".lua"
end

local add_tracked_file, update_tracked_file; do
    local tracked_files = {}
    function add_tracked_file(file_name, cl)
        for k, v in ipairs(tracked_files) do
            if v[1] == file_name and cl == v[3] then
                return
            end
        end
        table.insert(tracked_files, {file_name, file.Time(file_name, "LUA"), cl})
    end

    function update_tracked_file(file_name)
        for k, v in ipairs(tracked_files) do
            if v[1] == file_name then
                v[2] = 0
                return true
            end
        end
    end

    local time = file.Time
    util.AddNetworkString("Lau.Refresh_Client_File")
    timer.Create("Lau.ReloadFiles", 0.5, 0, function()
        for i = 1, #tracked_files do
            local v = tracked_files[i]
            local name = v[1]

            local last_update = time(name, "LUA")
            if last_update == 0 then continue end

            if v[2] ~= last_update then
                if v[3] then
                    timer.Create("Lau.Refresh_Client_File" .. name, 0.2, 1, function()
                        local code = Lau.RunFile(name, true)
                        if not code then return end

                        code = util.Compress(code)

                        net.Start("Lau.Refresh_Client_File")
                            net.WriteString(name)
                            net.WriteUInt(#code, 32)
                            net.WriteData(code, #code)
                        net.Send(player.GetAll())
                    end)
                else
                    Lau.RunFile(name)
                end

                v[2] = last_update
            end
        end
    end)
end

local Modules_Locals = ""
local Modules_Codes  = ""
function Lau.RunFile(file_name, no_run, no_lines, no_locals)
    local ls = handle_error(lex, file_name)
    if not ls then return end

    local tree = handle_error(parse, ls)
    if not tree then return end

    local code = generate(tree, no_lines)
    if not code then return end

    if not no_locals then
        code = Modules_Locals .. code
    end

    if no_run then
        return code
    end

    local full_path = "garrysmod/" .. get_path(file_name)
    full_path = ext_lua(full_path)

    gaceio.Write(full_path, code)
    local rets = {include(ext_lua(file_name))}
    gaceio.Delete(full_path)

    add_tracked_file(file_name)

    return unpack(rets, table.maxn(rets))
end

function Lau.AddCLFile(file_name)
    local full_path = get_path(file_name)
    if not full_path then return end

    local code = Lau.RunFile(file_name, true)
    if not code then code = " " end

    full_path = "garrysmod/" .. full_path
    full_path = ext_lua(full_path)

    if update_tracked_file(file_name) then
        return
    end

    gaceio.Write(full_path, code)
    AddCSLuaFile(ext_lua(file_name))

    gaceio.Delete(full_path)
    add_tracked_file(file_name, true)
end

local COMPILED_DIR = "garrysmod/data/lau_compiled/"
gaceio.CreateDir(COMPILED_DIR)

local COMPILING_DIR
function Lau.CompileFile(main_file, dir)
    if not isstring(main_file) or not GetExtensionFromFilename(main_file) then
        COMPILING_DIR = nil
        error("(Lau) You need to specify a valid file, got '" .. tostring(main_file) .. "'", 2)
    end

    if not file.Exists(main_file, "LUA") then
        COMPILING_DIR = nil
        error(string.format("(Lau) File '%s' doesn't exist.", main_file), 2)
    end

    local code = Lau.RunFile(main_file, true, nil, true)

    main_file = "lua/" .. StripExtension(main_file) .. ".lua"

    if dir then
        main_file = dir .. main_file
    end

    local new_lau
    local LAU_NAME = COMPILING_DIR
    if not LAU_NAME then
        LAU_NAME = "_" .. util.CRC(main_file .. math.random(900, 9999) .. RealTime()) .. os.time() .. "_"

        new_lau = Lau.RunFile("lau/compiled.lau", true, true, true) .. LAU_NAME .. "=Lau;" .. Modules_Codes
    else
        new_lau = "local Lau=" .. LAU_NAME .. ";"
    end

    local new_code = new_lau .. code

    file.CreateDir("lau_compiled/" .. GetPathFromFilename(main_file))
    gaceio.Write(COMPILED_DIR .. main_file, new_code)

    return LAU_NAME
end

local function loop_dir(dir, name)
    if dir:sub(#dir) ~= "/" then
        dir = dir .. "/"
    end

    local files, dirs = file.Find(dir .. "*", "LUA")

    for _, v in ipairs(files) do
        if GetExtensionFromFilename(v) == "lau" then
            Lau.CompileFile(dir .. v, name)
        end
    end

    for _, v in ipairs(dirs) do
        loop_dir(dir .. v, name)
    end
end

function Lau.CompileDir(name, main_file)
    if not isstring(name) or not file.IsDir(name, "LUA") then
        error(string.format("(Lau) Directory '%s' doesn't exist.", tostring(name)), 2)
    end

    local dir = (name .. "/"):match("^(.-)/") .. "/"

    COMPILING_DIR = Lau.CompileFile(main_file, dir)
    loop_dir(name, dir)
    COMPILING_DIR = nil
end

do
    for k, v in ipairs(Lau.Modules) do
        k = v.name

        local locals = ""
        for _, v2 in ipairs(v) do
            locals = locals .. "local " .. v2 .. "=Lau." .. v2 .. ";"
            Modules_Locals = Modules_Locals .. "local " .. v2 .. "=Lau." .. v2 .. ";"
        end
        local path = "lau/modules/" .. k .. ".lau"
        Lau.AddCLFile(path)
        Modules_Codes = Modules_Codes .. Lau.RunFile(path, true, true, true) .. locals
    end
    RunString(Modules_Codes)
end