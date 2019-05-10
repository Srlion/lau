Lau = {}

Lau.Modules = {
    {
        name = "colon_call",
        "__CALL__"
    },
    {
        name = "async",
        "__ASYNC__"
    },
    {
        name = "promise",
        "Promise"
    },
    {
        name = "await",
        "__AWAIT__",
        "await_failed"
    }
}

if CLIENT then
    function Lau.RunFile(name)
        if isstring(name) then
            name = name:StripExtension() .. ".lua";
        end
        include(name)
    end

    for k, v in ipairs(Lau.Modules) do
        Lau.RunFile("lau/modules/" .. v.name .. ".lau")
    end
end