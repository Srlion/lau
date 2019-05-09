local enum = include("lau/enum.lua")
local LITERAL, Literal = enum(
    function(s)
        return "'" .. s.value .. "'"
    end,
    function(s)
        return "Literal(" .. s.key:lower() .. "): " .. s
    end
)

Literal("Bool")
Literal("Number")
Literal("String")
Literal("Nil", "nil", "Literal(nil)")

return LITERAL