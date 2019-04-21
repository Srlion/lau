local enum = include("lau/enum.lua")
local KEYWORD, Keyword = enum(
	function(s)
		return "'" .. s.key:lower() .. "'"
	end,
	function(s)
		return "Keyword(" .. s .. ")"
	end
)

Keyword("Await")
Keyword("Class")
Keyword("Else")
Keyword("Enum")
Keyword("Extends")
Keyword("For")
Keyword("ForEach")
Keyword("Function")
Keyword("If")
Keyword("In")
Keyword("Let")
Keyword("New")
Keyword("Return")
Keyword("Static")
Keyword("While")
Keyword("Do")
Keyword("Break")
Keyword("Continue")
Keyword("Goto")

return KEYWORD