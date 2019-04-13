if _G[debug.getinfo(1).short_src] then return end _G[debug.getinfo(1).short_src] = true timer.Create("loading" .. debug.getinfo(1).short_src, 0.2, 1, function() _G[debug.getinfo(1).short_src] = false end)

-- s = function(s,)
--     print(s)
-- end
lau = {}

include("lau/modules/array.lua")
local parse = include("lau/parser.lua")
local lex_setup = include("lau/lexer.lua")
local generator = include("lau/test_generator.lua")
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

local function new_string_reader(src)
    local pos = 1
    local function reader()
        local chunk = string.sub(src, pos, pos + 4096 - 32)
        pos = pos + #chunk
        return #chunk > 0 && chunk || nil
    end
    return reader
end

function lau.compile(fileName, path)
    local ls = lex_setup(new_file_reader(fileName, path), fileName)
    local ast_builder = lua_ast.New()
    local ast_tree = parse(ast_builder, ls)
    local lua_code = generator(ast_tree, ast_builder, fileName)
    return lua_code
end

function lau.compile_string(str)
    local ls = lex_setup(new_string_reader(str), "compile_string")
    local ast_builder = lua_ast.New()
    local ast_tree = parse(ast_builder, ls)
    local lua_code = generator(ast_tree, ast_builder, "compile_string")
    return lua_code
end

function lau.include_file(fileName, path)
    path = path || "LUA"
    local code = lau.compile(fileName, "LUA")
    -- file.Write("tesst.txt", code)
    -- PrintType(code)
    return CompileString(code, fileName)()
end

print("\n\n\n\n\n\n\n\n\n\n\n-----------------------------------")
-- local ast = lau.compile_string("let s = ss")
-- PrintType(ast.body[1].right[1].body[1])

local lex = lau.include_file("lau/lexer/mod.lau")
local ls = lex("test.lau")
-- print(string.match("sss", "^(.*)[\r\n]"), true);

-- lau.include_file("lau/modules/async.lau")
-- lau.include_file("lau/modules/tenary.lau")
-- lau.include_file("lau/modules/promise.lau")
-- lau.include_file("lau/modules/await.lau")

-- local ast = lau.compile("test.lau", "LUA")
-- PrintType(ast)
-- file.Write("test.txt", ast)
-- CompileString(ast, "test.lau")()

local function startBench()
    local bench = include("bench.lua")
    print("\n\n\n\n\n------------------")
    jit.off();
    for i = 1, 6 do
        print()
        bench.Compare({
            function()
            end,
            function()
            end
        }, 99999)
    end
    jit.on();
    print("\n------------------")
end
concommand.Add("a", startBench)