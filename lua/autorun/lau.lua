Lau = {}

Lau.Modules = {
    colon_call  = {
        pos = 1,
        "__CALL__"
    },
    use = {
        pos = 2,
        "__USE__"
    },
    async = {
        pos = 3,
        "__ASYNC__"
    },
    await = {
        "__AWAIT__",
        "await_failed"
    },
    promise = {
        pos = 4,
        "Promise"
    }
}

if CLIENT then
    function Lau.RunFile(name)
        if isstring(name) then
            name = name:StripExtension() .. ".lua";
        end
        include(name)
    end

    for k, v in SortedPairsByMemberValue(Lau.Modules, "pos") do
        Lau.RunFile("lau/modules/" .. k .. ".lau")
    end
end