local enum = include("lau/enum.lua")
local KEYWORD, Keyword = enum(
    function(s)
        return "'" .. s.key:lower() .. "'"
    end,
    function(s)
        return "Keyword(" .. s .. ")"
    end
)

Keyword("Async")
Keyword("Await")
Keyword("Class")
Keyword("Else")
Keyword("Enum")
Keyword("Extends")
Keyword("For")
Keyword("ForEach")
Keyword("Fn", "'function'")
Keyword("If")
Keyword("In")
Keyword("Let")
Keyword("Return")
Keyword("Static")
Keyword("While")
Keyword("Do")
Keyword("Break")
Keyword("Continue")
Keyword("Goto")
Keyword("Use")

return KEYWORD