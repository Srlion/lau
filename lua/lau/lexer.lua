require('ffi')
local ffi = ffi

local band = bit.band
local strsub, strbyte, strchar = string.sub, string.byte, string.char

local ASCII_0, ASCII_9 = 48, 57
local ASCII_a, ASCII_f, ASCII_z = 97, 102, 122
local ASCII_A, ASCII_Z = 65, 90

local END_OF_STREAM = -1

local function table_to_true(tbl)
    for k, v in ipairs(tbl) do
        tbl[v] = true
        tbl[k] = nil
    end
    return tbl
end

local ReservedKeyword = table_to_true{
    'while', 'for', 'repeat',
    'in', 'break', 'until',
    'function', 'goto', 'if', 'else',
    'not', 'and', 'or',
    'return', 'do',
    'true', 'false', 'nil',
    -- glua
    'continue',
    -- lau
    'let', 'async', 'await', 'new', 'class', 'static', 'extends'
}

local ReplacedKeyword = table_to_true{
    'end', 'local', 'then', 'elseif'
}

local uint64, int64 = ffi.typeof('uint64_t'), ffi.typeof('int64_t')
local complex = ffi.typeof('complex')

local TokenSymbol = { TK_ge = '>=', TK_le = '<=' , TK_concat = '..', TK_eq = '==',
    TK_ne = '~=', TK_eof = '<eof>', TK_ar = '=>' }

local llex
local function token2str(tok)
    if string.match(tok, "^TK_") then
        return TokenSymbol[tok] or string.sub(tok, 4)
    else
        return tok
    end
end

local function error_lex(chunkname, tok, line, column, em, ...)
    local emfmt = string.format(em, ...)
    local msg = string.format("%s:%d:%d %s", chunkname, line, column + 1, emfmt)
    if tok then
        msg = string.format("%s near '%s'", msg, tok)
    end
    error(msg, 0)
end

local function lex_error(ls, token, em, ...)
    local tok
    if token == 'TK_name' or token == 'TK_string' or token == 'TK_number' then
        tok = ls.save_buf
    elseif token == 'TK_ao' then
        tok = ls.tokenval .. '='
    elseif token then
        tok = token2str(token)
    end
    error_lex(ls.chunkname, tok, ls.linenumber, ls.column, em, ...)
end

local function char_isident(c)
    if type(c) == 'string' then
        local b = strbyte(c)
        if b >= ASCII_0 and b <= ASCII_9 then
            return true
        elseif b >= ASCII_a and b <= ASCII_z then
            return true
        elseif b >= ASCII_A and b <= ASCII_Z then
            return true
        else
            return (c == '_')
        end
    end
    return false
end

local function char_isdigit(c)
    if type(c) == 'string' then
        local b = strbyte(c)
        return b >= ASCII_0 and b <= ASCII_9
    end
    return false
end

local function char_isspace(c)
    local b = strbyte(c)
    return b >= 9 and b <= 13 or b == 32
end

local function curr_is_newline(ls)
    local c = ls.current
    return (c == '\n' or c == '\r')
end

local function add_column(ls)
    if (curr_is_newline(ls)) then
        ls.column = 1
    else
        ls.column = ls.column + 1
    end
end

local function byte(ls, n)
    local k = ls.p + n
    return strsub(ls.data, k, k)
end

local function skip(ls, n)
    ls.n = ls.n - n
    ls.p = ls.p + n
end

local function pop(ls)
    local k = ls.p
    local c = strsub(ls.data, k, k)
    ls.p = k + 1
    ls.n = ls.n - 1
    add_column(ls)
    return c
end

local function fillbuf(ls)
    local data = ls:read_func()
    if not data then
        return END_OF_STREAM
    end
    ls.data, ls.n, ls.p, ls.column = data, #data, 1, 0
    return pop(ls)
end

local function nextchar(ls)
    local c = ls.n > 0 and pop(ls) or fillbuf(ls)
    ls.current = c
    return c
end

local function resetbuf(ls)
    ls.save_buf = ''
end

local function resetbuf_tospace(ls)
    ls.space_buf = ls.space_buf .. ls.save_buf
    ls.save_buf = ''
end

local function spaceadd(ls, str)
    ls.space_buf = ls.space_buf .. str
end

local function save(ls, c)
    ls.save_buf = ls.save_buf .. c
end

local function savespace_and_next(ls)
    ls.space_buf = ls.space_buf .. ls.current
    nextchar(ls)
end

local function save_and_next(ls)
    ls.save_buf = ls.save_buf .. ls.current
    nextchar(ls)
end

local function get_string(ls, init_skip, end_skip)
    return strsub(ls.save_buf, init_skip + 1, - (end_skip + 1))
end

local function get_space_string(ls)
    local s = ls.space_buf
    ls.space_buf = ''
    return s
end

local function inclinenumber(ls)
    local old = ls.current
    savespace_and_next(ls) -- skip `\n' or `\r'
    if curr_is_newline(ls) and ls.current ~= old then
        savespace_and_next(ls) -- skip `\n\r' or `\r\n'
    end
    ls.linenumber = ls.linenumber + 1
end

local function build_64int(str)
    local u = str[#str - 2]
    local x = (u == 117 and uint64(0) or int64(0))
    local i = 1
    while str[i] >= ASCII_0 and str[i] <= ASCII_9 do
        x = 10 * x + (str[i] - ASCII_0)
        i = i + 1
    end
    return x
end

-- Only lower case letters are accepted.
local function byte_to_hexdigit(b)
    if b >= ASCII_0 and b <= ASCII_9 then
        return b - ASCII_0
    elseif b >= ASCII_a and b <= ASCII_f then
        return 10 + (b - ASCII_a)
    else
        return -1
    end
end

local function build_64hex(str)
    local u = str[#str - 2]
    local x = (u == 117 and uint64(0) or int64(0))
    local i = 3
    while str[i] do
        local n = byte_to_hexdigit(str[i])
        if n < 0 then break end
        x = 16 * x + n
        i = i + 1
    end
    return x
end

local function strnumdump(str)
    local t = {}
    for i = 1, #str do
        local c = strsub(str, i, i)
        if char_isident(c) then
            t[i] = strbyte(c)
        else
            return nil
        end
    end
    return t
end

local function lex_number(ls)
    local lower = string.lower
    local xp = 'e'
    local c = ls.current
    if c == '0' then
        save_and_next(ls)
        local xc = ls.current
        if xc == 'x' or xc == 'X' then xp = 'p' end
    end
    while char_isident(ls.current) or ls.current == '.' or
        ((ls.current == '-' or ls.current == '+') and lower(c) == xp) do
        c = lower(ls.current)
        save(ls, c)
        nextchar(ls)
    end
    local str = ls.save_buf
    local x
    if strsub(str, -1, -1) == 'i' then
        local img = tonumber(strsub(str, 1, -2))
        if img then x = complex(0, img) end
    elseif strsub(str, -2, -1) == 'll' then
        local t = strnumdump(str)
        if t then
            x = xp == 'e' and build_64int(t) or build_64hex(t)
        end
    else
        x = tonumber(str)
    end
    if x then
        return x
    else
        lex_error(ls, 'TK_number', "malformed number")
    end
end

local function read_js_comment(ls)
    save_and_next(ls)
    if curr_is_newline(ls) then
        inclinenumber(ls)
    end
    while true do
        local c = ls.current
        if c == END_OF_STREAM then
            lex_error(ls, 'TK_eof', "unfinished long comment")
        elseif c == '*' then
            save_and_next(ls)
            if ls.current == "/" then
                save_and_next(ls) -- skip 2nd `['
                break
            end
        elseif c == '\n' or c == '\r' then
            save(ls, '\n')
            inclinenumber(ls)
            resetbuf(ls)
        else
            nextchar(ls)
        end
    end
end

local function add_to_literal_string(ls, tbl, tk, tkv)
    tbl = tbl || ls.template_string_tokens
    return table.insert(tbl, {line = ls.linenumber, tk, tkv})
end

local function read_till_char(ls)
    local reset = ls:fake_llex()
    ls.template_string_tokens = nil
    local tokens = {}
    add_to_literal_string(ls, tokens, "(")
    nextchar(ls)
    local skips = 0
    while true do
        local c = ls.current
        if c == END_OF_STREAM then
            lex_error(ls, 'TK_eof', "unfinished literal string")
        elseif c == "{" then
            add_to_literal_string(ls, tokens, "{")
            nextchar(ls)
            skips = skips + 1
        elseif c == "}" then
            if (skips > 0) then
                add_to_literal_string(ls, tokens, "}")
                nextchar(ls)
                skips = skips - 1
            else
                add_to_literal_string(ls, tokens, ")")
                nextchar(ls)
                local line, p = ls.linenumber, ls.p
                reset()
                ls.linenumber = line
                if (#tokens == 2) then
                    lex_error(ls, '}', "expecting expression")
                end
                return tokens, p
            end
        elseif c == '\n' or c == '\r' then
            inclinenumber(ls)
        elseif c == ' ' or c == '\t' or c == '\b' or c == '\f' then
            savespace_and_next(ls)
        else
            local tk, tkv = llex(ls)
            ls.template_string_tokens = nil
            add_to_literal_string(ls, tokens, tk, tkv)
        end
    end
end

local function read_long_string(ls, skip_save)
    if (!skip_save) then
        save_and_next(ls)
    end
    if curr_is_newline(ls) then
        save(ls, '\n')
        inclinenumber(ls)
    end
    while true do
        local c = ls.current
        if c == END_OF_STREAM then
            lex_error(ls, 'TK_eof', "unfinished literal string")
        elseif c == '`' then
            save_and_next(ls)
            break
        elseif c == '\n' or c == '\r' then
            save(ls, '\n')
            inclinenumber(ls)
        elseif (c == "$") then
            nextchar(ls)
            if ls.current == "{" then
                if (!ls.template_string_tokens) then
                    ls.template_string_tokens = {}
                end
                local str = get_string(ls, 1, 0)
                if str != "" then
                    add_to_literal_string(ls, nil, "TK_string", get_string(ls, 1, 0))
                    add_to_literal_string(ls, tokens, "TK_concat")
                end
                local tokens, p = read_till_char(ls)
                for k, v in ipairs(tokens) do
                    table.insert(ls.template_string_tokens, v)
                end
                while (ls.p != p) do
                    nextchar(ls)
                end
                resetbuf(ls)
                save(ls, "`")
                local pos = add_to_literal_string(ls, nil, "TK_concat")
                local str = read_long_string(ls, true)
                if str != nil then
                    if str != "" then
                        add_to_literal_string(ls, nil, "TK_string", str)
                    else
                        table.remove(ls.template_string_tokens, pos)
                    end
                end
                return
            else
                save(ls, '$')
                save_and_next(ls)
            end
        else
            save_and_next(ls)
        end
    end
    return get_string(ls, 1, 1)
end

local Escapes = {
    a = '\a', b = '\b', f = '\f', n = '\n', r = '\r', t = '\t',
    v = '\v',
}

local function hex_char(c)
    if string.match(c, '^%x') then
        local b = band(strbyte(c), 15)
        if not char_isdigit(c) then b = b + 9 end
        return b
    end
end

local function read_escape_char(ls)
    local c = nextchar(ls) -- Skip the '\\'.
    local esc = Escapes[c]
    if esc then
        save(ls, esc)
        nextchar(ls)
    elseif c == 'x' then -- Hexadecimal escape '\xXX'.
        local ch1 = hex_char(nextchar(ls))
        local hc
        if ch1 then
            local ch2 = hex_char(nextchar(ls))
            if ch2 then
                hc = strchar(ch1 * 16 + ch2)
            end
        end
        if not hc then
            lex_error(ls, 'TK_string', "invalid escape sequence")
        end
        save(ls, hc)
        nextchar(ls)
    elseif c == 'z' then -- Skip whitespace.
        nextchar(ls)
        while char_isspace(ls.current) do
            if curr_is_newline(ls) then inclinenumber(ls) else nextchar(ls) end
        end
    elseif c == '\n' or c == '\r' then
        save(ls, '\n')
        inclinenumber(ls)
    elseif c == '\\' or c == '\"' or c == '\'' then
        save(ls, c)
        nextchar(ls)
    elseif c == END_OF_STREAM then
    else
        if not char_isdigit(c) then
            lex_error(ls, 'TK_string', "invalid escape sequence")
        end
        local bc = band(strbyte(c), 15) -- Decimal escape '\ddd'.
        if char_isdigit(nextchar(ls)) then
            bc = bc * 10 + band(strbyte(ls.current), 15)
            if char_isdigit(nextchar(ls)) then
                bc = bc * 10 + band(strbyte(ls.current), 15)
                if bc > 255 then
                    lex_error(ls, 'TK_string', "invalid escape sequence")
                end
                nextchar(ls)
            end
        end
        save(ls, strchar(bc))
    end
end

local function read_string(ls, delim)
    save_and_next(ls)
    while ls.current ~= delim do
        local c = ls.current
        if c == END_OF_STREAM then
            lex_error(ls, 'TK_eof', "unfinished string")
        elseif c == '\n' or c == '\r' then
            lex_error(ls, 'TK_string', "unfinished string")
        elseif c == '\\' then
            read_escape_char(ls)
        else
            save_and_next(ls)
        end
    end
    save_and_next(ls) -- skip delimiter
    return get_string(ls, 1, 1)
end

local function skip_line(ls)
    while not curr_is_newline(ls) and ls.current ~= END_OF_STREAM do
        savespace_and_next(ls)
    end
end

local assignment_operators = {
    ["+"] = true,
    ["-"] = true,
    ["*"] = true,
    ["^"] = true,
    ["%"] = true
}

function llex(ls)
    local template_string_tokens = ls.template_string_tokens
    if (template_string_tokens && #template_string_tokens > 0) then
        ls.linenumber = template_string_tokens[1].line
        return unpack(table.remove(template_string_tokens, 1))
    end
    resetbuf(ls)
    while true do
        local current = ls.current
        if char_isident(current) then
            if char_isdigit(current) then -- Numeric literal.
                return 'TK_number', lex_number(ls)
            end
            repeat
                save_and_next(ls)
            until not char_isident(ls.current)
            local s = get_string(ls, 0, 0)
            local reserved = ReservedKeyword[s]
            if reserved then
                return 'TK_' .. s
            elseif (ReplacedKeyword[s]) then
                return 'TK_name', '__' .. s .. '__'
            else
                return 'TK_name', s
            end
        end
        if current == '\n' or current == '\r' then
            inclinenumber(ls)
        elseif current == ' ' or current == '\t' or current == '\b' or current == '\f' then
            savespace_and_next(ls)
            -- nextchar(ls)
        elseif current == '/' then
            nextchar(ls)
            if ls.current == '/' then
                nextchar(ls)
                skip_line(ls)
            elseif ls.current == "*" then
                read_js_comment(ls)
                resetbuf_tospace(ls)
            elseif ls.current == '=' then
                nextchar(ls)
                return 'TK_ao', '/'
            else
                return '/'
            end
        elseif current == '`' then
            local str = read_long_string(ls)
            if str == nil then
                return llex(ls)
            end
            return 'TK_string', str
        elseif current == '=' then
            nextchar(ls)
            if ls.current == '>' then nextchar(ls); return 'TK_ar' end -- arrow functions
            if ls.current ~= '=' then return current else nextchar(ls); return 'TK_eq' end
        elseif assignment_operators[current] then
            nextchar(ls)
            if (current == '+' or current == '-') and ls.current == current then nextchar(ls) return current .. current end
            if ls.current == '&' then nextchar(ls); return 'TK_&&' end
            if ls.current == '=' then nextchar(ls); return 'TK_ao', current else return current end
        elseif current == '<' then
            nextchar(ls)
            if ls.current ~= '=' then return '<' else nextchar(ls); return 'TK_le' end
        elseif current == '>' then
            nextchar(ls)
            if ls.current ~= '=' then return '>' else nextchar(ls); return 'TK_ge' end
        elseif current == '~' then
            nextchar(ls)
            if ls.current ~= '=' then return '~' else nextchar(ls); return 'TK_ne' end
        elseif current == '!' then -- glua
            nextchar(ls)
            if (ls.current == '=') then
                nextchar(ls)
                return 'TK_!='
            end
            return 'TK_!'
        elseif current == '&' || current == '|' then
            nextchar(ls)
            if (ls.current ~= current) then
                return current
            end
            nextchar(ls)
            return 'TK_' .. current:rep(2)
        elseif current == ':' then
            nextchar(ls)
            if ls.current ~= ':' then return ':' else nextchar(ls); return 'TK_label' end
        elseif current == '"' or current == "'" then
            local str = read_string(ls, current)
            return 'TK_string', str
        elseif current == '.' then
            save_and_next(ls)
            if ls.current == '.' then
                nextchar(ls)
                if ls.current == '.' then
                    nextchar(ls)
                    return 'TK_dots' -- ...
                elseif ls.current == '=' then
                    nextchar(ls)
                    return 'TK_ao', '..'
                end
                return 'TK_concat'
            elseif not char_isdigit(ls.current) then
                return '.'
            else
                return 'TK_number', lex_number(ls)
            end
        elseif current == END_OF_STREAM then
            return 'TK_eof'
        else
            nextchar(ls)
            return current -- Single-char tokens (+ - / ...).
        end
    end
end

local Lexer = {
    token2str = token2str,
    error = lex_error,
}

function Lexer.next(ls)
    ls.lastline = ls.linenumber
    if ls.tklookahead == 'TK_eof' then -- No lookahead token?
        ls.previousToken, ls.previousTokenVal = ls.token, ls.tokenval
        ls.token, ls.tokenval = llex(ls) -- Get nextchar token.
        ls.space = get_space_string(ls)
    else
        ls.previousToken, ls.previousTokenVal = ls.token, ls.tokenval
        ls.token, ls.tokenval = ls.tklookahead, ls.tklookaheadval
        ls.linenumber = ls.lookaheadline
        ls.space = ls.spaceahead
        ls.tklookahead = 'TK_eof'
    end
end

function Lexer.lookahead(ls)
    assert(ls.tklookahead == 'TK_eof')
    local line = ls.linenumber
    local lastline = ls.lastline
    ls.tklookahead, ls.tklookaheadval = llex(ls)
    ls.spaceahead = get_space_string(ls)
    ls.lookaheadline = ls.linenumber
    ls.linenumber = line
    ls.lastline = lastline
    return ls.tklookahead
end

Lexer.fake_llex = function(ls) -- hehe
    local old_ls = table.Copy(ls)
    return function()
        table.Empty(ls)
        for k, v in pairs(old_ls) do
            ls[k] = v
            old_ls[k] = nil
        end
        old_ls = nil
    end
end

local LexerClass = { __index = Lexer }

local function lex_setup(read_func, chunkname)
    local header = false
    local ls = {
        n = 0,
        tklookahead = 'TK_eof', -- No look-ahead token.
        linenumber = 1,
        lastline = 1,
        read_func = read_func,
        chunkname = chunkname,
        space_buf = ''
    }
    nextchar(ls)
    if ls.current == '\xef' and ls.n >= 2 and
        byte(ls, 0) == '\xbb' and byte(ls, 1) == '\xbf' then -- Skip UTF-8 BOM (if buffered).
        ls.n = ls.n - 2
        ls.p = ls.p + 2
        nextchar(ls)
        header = true
    end
    if ls.current == '#' then
        repeat
            nextchar(ls)
            if ls.current == END_OF_STREAM then return ls end
        until curr_is_newline(ls)
        inclinenumber(ls)
        header = true
    end
    return setmetatable(ls, LexerClass)
end

return lex_setup