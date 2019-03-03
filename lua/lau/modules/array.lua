if _G[debug.getinfo(1).short_src] then return end _G[debug.getinfo(1).short_src] = true timer.Create("loading" .. debug.getinfo(1).short_src, 0.2, 1, function() _G[debug.getinfo(1).short_src] = false end)


local select = select
local table = table
local remove = table.remove
local insert = table.insert
local maxn = table.maxn

local setmetatable = setmetatable

Array = {}
function Array.isArray(t)
    return t.____isarray || false
end

-- Array Methods
local Array_m = {}
Array_m.__index = Array_m
Array_m.____isarray = true

function Array_m:push(...)
    local args = {...}
    for i = 1, select("#", ...) do
        insert(self, i, args[i])
    end
    return maxn(self)
end

function Array_m:pop()
    return remove(self)
end

function Array_m:shift()
    return remove(self, 1)
end

function Array_m:unshift(value)
    return insert(self, 1, value)
end

function Array_m:indexOf(value)
    for i = 1, maxn(self) do
        if (value == self[i]) then
            return i
        end
    end
    return -1
end

function Array_m:splice(index, count, ...)
    local length = maxn(self)
    local i2, step = 0, 1
    if (index < 0) then
        if (index * -1 > length) then
            index = 0
        else
            index = length + index + 1
        end
        step = -1
        i2 = length + 1
    elseif (index > length) then
        index = length
    end

    -- if true then return end
    local args = {...}
    for i = index, i2 - count, step do
        i2 = i2 + step
        self[i], args[i2] = args[i2], self[i]
    end
end

function Array_m:reverse()
    local p = #self
    for i = 1, p * 0.5 do
        self[i], self[p] = self[p], self[i]
        p = p - 1
    end
end

function Array_m:map(cb)
    local newArray = newArray({})
    for i = 1, maxn(self) do
        local v = self[i]
        newArray[i] = (v != nil) && cb(v) || nil
    end
    return newArray
end

function newArray(tbl)
    return setmetatable(tbl, Array_m)
end

local s = newArray({})
for i = 1, 5 do
    s[i] = i
end