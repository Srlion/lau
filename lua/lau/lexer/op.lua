local enum = include("lau/enum.lua")
local OP, Op = enum(
	nil,
	function(s)
		return "Op(" .. s .. ")"
	end
)

// Assignment
Op("Assign", "'='")
Op("AddAssign", "'+='")
Op("SubAssign", "'-='")
Op("DivAssign", "'/='")
Op("MulAssign", "'*='")
Op("ModAssign", "'%='")
Op("PowAssign", "'^='")
Op("ConAssign", "'..='")

// Arithmetic
Op("Add", "'+'")
Op("Sub", "'-'")
Op("Div", "'/'")
Op("Mul", "'*'")
Op("Mod", "'%'")
Op("Pow", "'^'")

// Comparison
Op("Eq", "'=='")
Op("Ne", "'!='")
Op("Gt", "'>'")
Op("Lt", "'<'")
Op("GtEq", "'>='")
Op("LtEq", "'<='")

// Logical
Op("LAnd", "'&&'")
Op("LOr", "'||'")
Op("Not", "'!'")

// Misc
Op("Ellipsis", "'...'")
Op("Concat", "'..'")
Op("Cond", "'?'")
Op("Dot", "'.'")
Op("Len", "'#'")

return OP