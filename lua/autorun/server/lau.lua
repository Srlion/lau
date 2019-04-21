if _G[debug.getinfo(1).short_src] then return end _G[debug.getinfo(1).short_src] = true timer.Create("loading" .. debug.getinfo(1).short_src, 0.2, 1, function() _G[debug.getinfo(1).short_src] = false end)

-- s = function(s,)
--     print(s)
-- end

print("\n\n\n\n\n\n\n\n\n\n\n-----------------------------------")

local function handle_error(fn, ...)
    local status, v = pcall(fn, ...)

    if (status == false) then
        return ErrorNoHalt("[ERROR] " .. v .. "\n")
    end

    return v
end

Lau = {}
local lex = include("lau/lexer/mod.lua")
local parse = include("lau/parser/parser.lua")
local generate = include("lau/generator.lua")

function Lau.RunFile(file_name, path, no_run, no_lines, addon_name)
    local ls = handle_error(lex, file_name, path)
    if (!ls) then return end

    local tree = handle_error(parse, ls)
    if (!tree) then return end

    code = generate(tree, no_lines, addon_name)

    if no_run then return code end

    print(code)
    CompileString(code, file_name)
end

Lau.RunFile("test.lau")

function Lau.Addon(addon_name)
    return function(file_name, path, no_run, no_lines)
        return Lau.RunFile(file_name, path, no_run, no_lines, addon_name)
    end
end

concommand.Add("a", function()
    local bench = include("bench.lua")
    print("\n\n\n\n\n------------------")
    jit.off()
    for i = 1, 6 do
        print()
        bench.Compare({
            function()
            end,
            function()
            end
        }, 99999)
    end
    jit.on()
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