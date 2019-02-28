if _G[debug.getinfo(1).short_src] then return end _G[debug.getinfo(1).short_src] = true timer.Create("loading" .. debug.getinfo(1).short_src, 0.2, 1, function() _G[debug.getinfo(1).short_src] = false end)

lau = {}

local parse = include("lau/parser.lua")
local lex_setup = include("lau/lexer.lua")
local generator = include("lau/generator.lua")
local lua_ast = include("lau/ast.lua")

local function new_file_reader(filename, path)
    filename = tostring(filename)
    assert(isstring(filename), "invalid file name `" .. filename .. "`")
    local f = assert(file.Open(filename, "r", path), "cannot open file " .. filename)
    local function reader()
        local e = f:Read(4096 - 32)
        if (e == nil) then
            f:Close()
        end
        return e
    end
    return reader
end

local strsub = string.sub
local function new_string_reader(src)
    local pos = 1
    local function reader()
        local chunk = strsub(src, pos, pos + 4096 - 32)
        pos = pos + #chunk
        return #chunk > 0 && chunk || nil
    end
    return reader
end

function lau.compile(fileName, path)
    local ls = lex_setup(new_file_reader(fileName, path), fileName)
    local ast_builder = lua_ast.New()
    local ast_tree = parse(ast_builder, ls)
    local lua_code = generator(ast_tree, fileName)
    return lua_code
end

function lau.compile_string(str)
    local ls = lex_setup(new_string_reader(str), "compile_string")
    local ast_builder = lua_ast.New()
    local ast_tree = parse(ast_builder, ls)
    local lua_code = generator(ast_tree, "compile_string")
    return lua_code
end

print("\n\n\n\n\n\n\n\n\n\n\n-----------------------------------")
local ast = lau.compile("test.js", "LUA")
-- local ast = lau.compile_string("let s = ss")
-- PrintType(ast)

local table = table
local remove = table.remove
local insert = table.insert
local maxn = table.maxn
local getmetatable = getmetatable

local Array = {}

function Array.isArray(t)
    return getmetatable(t).__isarray && true || false
end

function Array.push(t, ...)
    local args = {...}
    for i = 1, select("#", ...) do
        insert(t, i, args[i])
    end
    return #t
end

function Array.pop(t)
    return remove(t)
end

function Array.shift(t)
    return remove(t, 1)
end

function Array.unshift(t, v)
    return insert(t, 1, v)
end

function Array.indexOf(t, v)
    for i = 1, maxn(t) do
        if (v == t[i]) then
            return i
        end
    end
    return -1
end

local e = {}
Array.push(e, 1, nil, 3)
PrintType(Array.indexOf(e, nil))

do
    local setmetatable = setmetatable
    local __isarray = {__isarray = true}
    function newArray(tbl)
        return setmetatable(tbl, __isarray)
    end
end

-- local function startBench()
--     local bench = include("bench.lua")
--     print("\n\n\n\n\n------------------")
--     for i = 1, 6 do
--         print()
--         bench.Compare({
--             function()
--             end,
--             function()
--             end
--         }, 9999)
--     end
--     print("\n------------------")
-- end
-- concommand.Add("a", startBench)