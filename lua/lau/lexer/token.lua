local enum = include("lau/enum.lua")

local Msg = Msg
local tostring = tostring
local setfenv = setfenv

local TOKEN, Token
TOKEN, Token = enum(
    nil,
    function(s)
        setfenv(1, TOKEN)

        local msg = "Token("

        if (s == Ident) then
            msg = msg .. "[ident: " .. s.value .. "]"
        else
            msg = msg .. s
        end

        msg = msg .. ")"

        return msg
    end
)

Token("Arrow", "'=>'")
Token("Colon", "':'")
Token("Label", "'::'")
Token("Comma", "','")
Token("Semicolon", "';'")
Token("LParens", "'('")
Token("LBrace", "'{'")
Token("LBracket", "'['")
Token("RBracket", "']'")
Token("RBrace", "'}'")
Token("RParens", "')'")
Token("EOF", "'EOF'")

Token("Ident", function(s)
    return "'" .. tostring(s.value) .. "'"
end)

return TOKEN