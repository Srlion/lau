if _G[debug.getinfo(1).short_src] then return end _G[debug.getinfo(1).short_src] = true timer.Create("loading" .. debug.getinfo(1).short_src, 0.2, 1, function() _G[debug.getinfo(1).short_src] = false end)

-- s = function(s,)
--     print(s)
-- end
lau = {}

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
local generate, new_generate = include("lau/generator.lua")

local FILES = false
local function lau_run_file(name, path, text)
    local ls = handle_error(lex, name)
    if (!ls) then return end

    local tree = handle_error(parse, ls)
    if (!tree) then return end

    local code
    if (FILES) then
        code = FILES
    else
        code = generate(tree, name, false)
    end

    print(code)
    CompileString(code, name)
end

lau_run_file("test.lau")

local function lau_run_files(name)
    return function(tree)
        return lau_run_file(name, path, text)
    end
end

concommand.Add("a", function()
    local bench = include("bench.lua")
    print("\n\n\n\n\n------------------")
    for i = 1, 6 do
        print()
        bench.Compare({
            function()
                type(nil)
            end,
            -- function()
            -- end
        }, 999999)
    end
    print("\n------------------")
end)

concommand.Add("e", function()
    print("\n\n\n\n\n------------------")
    local shit = os.clock()
    for i = 1, 44 do
        local ls = handle_error(lex, "test.lau")
        if (ls) then
            local tree = handle_error(parse, ls)
            if (tree) then
                -- local code = generate(tree)
                -- print(code)
                -- CompileString(code, "test.lau")()
            end
        end
    end
    print(os.clock() - shit)
    print("\n------------------")
end)