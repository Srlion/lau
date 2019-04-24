Lau = {}

if CLIENT then
    function Lau.RunFile(name)
        if isstring(name) then
            name = name:StripExtension() .. ".lua";
        end
        include(name)
    end
end