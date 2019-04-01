--[[
    Examples:
    local async = include("async.lua")

    local Sleep = function(t)
        return async.Promise(function(rs)
            timer.Simple(t, rs)
        end)
    end

    local printAfterTime = async(function(time)
        Sleep(time)
        print("oOoOoO printed after " .. time .. " second(s)")
    end)

    printAfterTime(1)

    local fetchGoogle = function() -- You don't have to use async here
        return async.httpFetch("http://google.com")
    end

    async(function(...)
        local status, body, len, headers, code = fetchGoogle()
        if (!status) then print(body) return end

        print(body, len, headers, code)
    end)()
]]--

AddCSLuaFile()

local unpack = unpack
local select = select

local coroutine_resume  = coroutine.resume
local coroutine_yield   = coroutine.yield
local coroutine_running = coroutine.running
local coroutine_create  = coroutine.create

local getCoroutine
do
    local table_maxn = table.maxn
    local cached = {}
    local count = 0
    local function pack(...)
        return select("#", ...), {...}
    end
    function getCoroutine(fn, ...) -- some stuff from http://lua-users.org/lists/lua-l/2013-07/msg00752.html
        local rets_n, rets
        if (count > 0) then
            rets_n, rets = pack(coroutine_resume(cached[count], fn, ...))
            cached[count] = nil
            count = count - 1
            if (not rets[1]) then
                error(rets[2], 0)
            end
            return unpack(rets, 2, rets_n)
        else
            local _cached
            _cached = coroutine_create(function(...)
                local arg = {...}
                while true do
                    arg[1](unpack(arg, 2, table_maxn(arg)))
                    count = count + 1
                    cached[count] = _cached
                    arg = {coroutine_yield()}
                end
            end)
            rets_n, rets = pack(coroutine_resume(_cached, fn, ...))
            if (not rets[1]) then
                error(rets[2], 0)
            end
            return unpack(rets, 2, rets_n)
        end
    end
end
local LAU_ASYNC = {}
LAU_ASYNC.__index = LAU_ASYNC
setmetatable(LAU_ASYNC, {
    __call = function(_, fn)
        return function(...)
            if (coroutine_running()) then
                return fn(...)
            else
                return getCoroutine(fn, ...)
            end
        end
    end
})
return LAU_ASYNC