if _G[debug.getinfo(1).short_src] then return end _G[debug.getinfo(1).short_src] = true timer.Create("loading" .. debug.getinfo(1).short_src, 0.2, 1, function() _G[debug.getinfo(1).short_src] = false end)

require("gaceio")

print("\n\n\n\n\n\n\n\n\n\n\n-----------------------------------")

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
    return name:StripExtension() .. ".lua"
end

local add_tracked_file; do
    local time = file.Time

    local tracked_files = {}
    function add_tracked_file(file_name, cl)
        tracked_files[file_name] = {time(file_name, "LUA"), cl}
    end

    timer.Create("Lau.ReloadFiles", 0.2, 0, function()
        for name, v in pairs(tracked_files) do
            local last_update = time(name, "LUA")
            if v[1] ~= last_update then
                if v[2] then
                    Lau.AddCLFile(name)
                else
                    Lau.RunFile(name)
                end

                v[1] = last_update
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

    gaceio.Write(full_path, code)
    AddCSLuaFile(ext_lua(file_name))
    gaceio.Delete(full_path)

    add_tracked_file(file_name, true)
end

local COMPILED_DIR = "garrysmod/data/lau_compiled/"
gaceio.CreateDir(COMPILED_DIR)

local COMPILING_DIR
function Lau.CompileFile(main_file, dir)
    if not isstring(main_file) or not main_file:GetExtensionFromFilename() then
        COMPILING_DIR = nil
        error("(Lau) You need to specify a valid file, got '" .. tostring(main_file) .. "'", 2)
    end

    if not file.Exists(main_file, "LUA") then
        COMPILING_DIR = nil
        error(string.format("(Lau) File '%s' doesn't exist.", main_file), 2)
    end

    local code = Lau.RunFile(main_file, true)
    if not code then code = " " end

    main_file = "lua/" .. main_file:StripExtension() .. ".lua"

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

    file.CreateDir("lau_compiled/" .. main_file:GetPathFromFilename())
    gaceio.Write(COMPILED_DIR .. main_file, new_code)

    return LAU_NAME
end

local function loop_dir(dir, name)
    if dir:sub(#dir) ~= "/" then
        dir = dir .. "/"
    end

    local files, dirs = file.Find(dir .. "*", "LUA")

    for _, v in ipairs(files) do
        if v:GetExtensionFromFilename() == "lau" then
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
    for k, v in SortedPairsByMemberValue(Lau.Modules, "pos") do
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

do
    local SV, CL, SH = 1, 2, 3
    local function load_dir(dir, state)
        dir = dir .. "/"

        for _, v in ipairs(file.Find(dir .. "*", "LUA")) do
            if v:GetExtensionFromFilename() ~= "lau" then continue end

            if state == CL or SH then
                Lau.AddCLFile(dir .. v)
                Lau.CompileFile(dir .. v)
            end

            if state == SV or SH then
                Lau.RunFile(dir .. v)
                Lau.CompileFile(dir .. v)
            end
        end
    end

    load_dir("autorun", SH)
    load_dir("autorun/server", SV)
    load_dir("autorun/client", CL)
end

-- local function startBench()
--     local bench = include("bench.lua")
--     print("\n\n\n\n\n------------------")
--     jit.off();
--     for i = 1, 6 do
--         print()
--         bench.Compare({
--             function()
--             end,
--             function()
--             end
--         }, 999999)
--     end
--     jit.on();
--     print("\n------------------")
-- end
-- concommand.Add("a", startBench)