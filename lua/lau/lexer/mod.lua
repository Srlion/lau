Lau.lexer = {}

lexer = Lau.lexer
lexer.Keyword = include("lau/lexer/keyword.lua")
lexer.Literal = include("lau/lexer/literal.lua")
lexer.Op = include("lau/lexer/op.lua")
lexer.Token = include("lau/lexer/token.lua")

local f_Open = file.Open
local function new_file_reader(filename, path)
    if (path == nil) then
        path = "LUA"
    end

    if not isstring(filename) then
        error("invalid file name (" .. filename .. ")", 2)
    end

    local f = f_Open(filename, "rb", path)
    local size = f:Size()
    return f:Read(size), size, f:Close()
end

local lex_setup = include("lau/lexer/main.lua")
return function(filename, path)
    return lex_setup(filename, new_file_reader(filename, path))
end