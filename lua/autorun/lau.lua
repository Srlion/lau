Lau = {}

if CLIENT then
    function Lau.RunFile(name)
        if isstring(name) then
            name = name:StripExtension() .. ".lua";
        end
        include(name)
    end

    Lau.Modules = {
        colon_call  = {
            pos = 1
        },
        async = {
            pos = 2
        },
        await = {},
        promise = {
            pos = 4
        }
    }

    for k, v in SortedPairsByMemberValue(Lau.Modules, "pos") do
        Lau.RunFile("lau/modules/" .. k .. ".lau")
    end
end