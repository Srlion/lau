return function(__display, __debug)
	local ENUM = {}
	local tostring = tostring
	local setmetatable = setmetatable
	local debug_getinfo = debug.getinfo

	return ENUM, function(_enum, display, debug)
		if (!display) then
			display = __display
		end

		if (!debug) then
			debug = __debug
		end

		local function __tostring(s)
			local func = debug_getinfo(2, "f").func

			if (func == print || func == MsgC || func == Msg) then
				if (isstring(debug)) then
					return debug
				else
					return debug(s)
				end
			else
				if (isstring(display)) then
					return display
				else
					return display(s)
				end
			end
		end

		local enum = {
			key = _enum
		}

		enum.__tostring = __tostring

		function enum.__eq()
			return true
		end

		function enum.__concat(v1, v2)
			if (v1 == enum) then
				return tostring(v1) .. v2
			else
				return v1 .. tostring(v2)
			end
		end

		setmetatable(enum, {
			__call = function(s, v)
				return setmetatable({
					key = s.key,
					value = v
				}, s)
			end,
			__tostring = __tostring,
			__eq = enum.__eq,
			__concat = enum.__concat,
			key = _enum
		})

		ENUM[_enum] = enum

		return enum
	end
end