require("Lau")

do
    local SV, CL, SH = 1, 2, 3
    local function load_dir(dir, state)
        dir = dir .. "/"

        for _, v in ipairs(file.Find(dir .. "*", "LUA")) do
            if v:GetExtensionFromFilename() ~= "lau" then continue end

            if state == CL or SH then
                Lau.AddCLFile(dir .. v)
                Lau.CompileFile(dir .. v)
            end

            if state == SV or SH then
                Lau.RunFile(dir .. v)
                Lau.CompileFile(dir .. v)
            end
        end
    end

    load_dir("autorun", SH)
    load_dir("autorun/server", SV)
    load_dir("autorun/client", CL)
end