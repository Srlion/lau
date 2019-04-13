require('ffi')

local band = bit.band
local isstring = isstring
local tonumber = tonumber
local strsub, strbyte, strchar = string.sub, string.byte, string.char

local ASCII_0, ASCII_9 = 48, 57
local ASCII_a, ASCII_f, ASCII_z = 97, 102, 122
local ASCII_A, ASCII_Z = 65, 90

local END_OF_STREAM = -1

local lexer = lau.lexer
local Keyword = lexer.Keyword;
local Literal = lexer.Literal;
local Op = lexer.Op;
local Token = lexer.Token;
local Keyword_KEYS = {}
for k, v in pairs(Keyword) do
    Keyword_KEYS[k:lower()] = v
end

local uint64, int64 = ffi.typeof("uint64_t"), ffi.typeof("int64_t")
local complex = ffi.typeof("complex")

local function token2str(tok)
    if isstring(tok) && string.match(tok, "^TK_") then
        return string.sub(tok, 4)
    else
        return tok
    end
end

local function error_lex(ls, chunkname, tok, line, em, ...)
    local column = ls.p
    if (line > 1) then
        local temp, pos = ls.data:sub(1, column - 1)
        for i = -1, -math.huge, -1 do
            pos = temp:find("[\r\n]", i);
            if (pos) then
                break;
            end
        end
        column = column - pos;
    end
    local emfmt = string.format(em, ...)
    local msg = string.format("%s:%d:%d %s", chunkname, line, column - 1, emfmt)
    if tok then
        msg = string.format("%s near (%s)", msg, tok)
    end
    error(msg, 0)
end

local ReverseEscapes
local function lex_error(ls, token, em, ...)
    local tok
    if token == "TK_string" then
        tok = ls.save_buf:gsub(".", ReverseEscapes)
    elseif token == 'TK_name' || token == 'TK_number' then
        tok = ls.save_buf
    elseif token then
        tok = token2str(token)
    end
    error_lex(ls, ls.chunkname, tok, ls.linenumber, em, ...)
end

local function char_isident(c)
    if isstring(c) then
        local b = strbyte(c)
        if b >= ASCII_0 && b <= ASCII_9 then
            return true
        elseif b >= ASCII_a && b <= ASCII_z then
            return true
        elseif b >= ASCII_A && b <= ASCII_Z then
            return true
        else
            return (c == '_')
        end
    end
    return false
end

local function char_isdigit(c)
    if isstring(c) then
        local b = strbyte(c)
        return b >= ASCII_0 && b <= ASCII_9
    end
    return false
end

local function char_isspace(c)
    local b = strbyte(c)
    return b >= 9 && b <= 13 || b == 32
end

local function byte(ls, n)
    local k = ls.p + n
    return strsub(ls.data, k, k)
end

local function pop(ls)
    local k = ls.p
    local c = strsub(ls.data, k, k)
    ls.p = k + 1
    ls.n = ls.n - 1
    return c
end

local function nextchar(ls)
    local c = ls.n > 0 && pop(ls) || END_OF_STREAM
    ls.current = c
    return c
end

local function check_nextchar(ls)
    local k = ls.p
    return strsub(ls.data, k, k)
end

local function curr_is_newline(ls)
    local c = ls.current
    return (c == '\n' || c == '\r')
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
    savespace_and_next(ls) // skip `\n' or `\r'
    if curr_is_newline(ls) && ls.current ~= old then
        savespace_and_next(ls) // skip `\n\r' or `\r\n'
    end
    ls.linenumber = ls.linenumber + 1
end

local function skip_sep(ls)
    local count = 0
    local s = ls.current
    assert(s == '[' || s == ']')
    save_and_next(ls)
    while ls.current == '=' do
        save_and_next(ls)
        count = count + 1
    end
    return ls.current == s && count || (-count - 1)
end

local function build_64int(str)
    local u = str[#str - 2]
    local x = (u == 117 && uint64(0) || int64(0))
    local i = 1
    while str[i] >= ASCII_0 && str[i] <= ASCII_9 do
        x = 10 * x + (str[i] - ASCII_0)
        i = i + 1
    end
    return x
end

// Only lower case letters are accepted.
local function byte_to_hexdigit(b)
    if b >= ASCII_0 && b <= ASCII_9 then
        return b - ASCII_0
    elseif b >= ASCII_a && b <= ASCII_f then
        return 10 + (b - ASCII_a)
    else
        return -1
    end
end

local function build_64hex(str)
    local u = str[#str - 2]
    local x = (u == 117 && uint64(0) || int64(0))
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
        if xc == 'x' || xc == 'X' then xp = 'p' end
    end
    while char_isident(ls.current) || ls.current == '.' ||
        ((ls.current == '-' || ls.current == '+') && lower(c) == xp) do
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
            x = xp == 'e' && build_64int(t) || build_64hex(t)
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

local function read_long_string(ls, sep, ret_value)
    save_and_next(ls) // skip 2nd `['
    if curr_is_newline(ls) then // string starts with a newline?
        inclinenumber(ls) // skip it
    end
    while true do
        local c = ls.current
        if c == END_OF_STREAM then
            lex_error(ls, Token.EOF, ret_value && "unfinished long string" || "unfinished long comment")
        elseif c == ']' then
            if skip_sep(ls) == sep then
                save_and_next(ls) // skip 2nd `['
                break
            end
        elseif c == '\n' || c == '\r' then
            save(ls, '\n')
            inclinenumber(ls)
            if !ret_value then
                resetbuf(ls) // avoid wasting space
            end
        else
            if ret_value then save_and_next(ls)
            else nextchar(ls) end
        end
    end
    if ret_value then
        return get_string(ls, 2 + sep, 2 + sep)
    end
end

local function read_long_comment(ls)
    save_and_next(ls)
    if curr_is_newline(ls) then
        inclinenumber(ls)
    end
    while true do
        local c = ls.current
        if c == END_OF_STREAM then
            ls.p = ls.p + 1
            lex_error(ls, "TK_string", "unfinished long comment")
        elseif c == '*' then
            save_and_next(ls)
            if ls.current == "/" then
                save_and_next(ls) // skip 2nd `['
                break
            end
        elseif c == '\n' || c == '\r' then
            save(ls, '\n')
            inclinenumber(ls)
            resetbuf(ls)
        else
            nextchar(ls)
        end
    end
end

local function hex_char(c)
    if string.match(c, '^%x') then
        local b = band(strbyte(c), 15)
        if !char_isdigit(c) then b = b + 9 end
        return b
    end
end

local Escapes = {a = '\a', b = '\b', f = '\f', n = '\n', r = '\r', t = '\t', v = '\v'}
ReverseEscapes = {}
for k, v in pairs(Escapes) do
    ReverseEscapes[v] = "\\" .. k
end
local function read_escape_char(ls)
    local c = nextchar(ls) // Skip the '\\'.
    local esc = Escapes[c]
    if esc then
        save(ls, esc)
        nextchar(ls)
    elseif c == 'x' then // Hexadecimal escape '\xXX'.
        local ch1 = hex_char(nextchar(ls))
        local hc
        if ch1 then
            local ch2 = hex_char(nextchar(ls))
            if ch2 then
                hc = strchar(ch1 * 16 + ch2)
            end
        end
        if !hc then
            lex_error(ls, 'TK_string', "invalid escape sequence")
        end
        save(ls, hc)
        nextchar(ls)
    elseif c == 'z' then // Skip whitespace.
        nextchar(ls)
        while char_isspace(ls.current) do
            if curr_is_newline(ls) then inclinenumber(ls) else nextchar(ls) end
        end
    elseif c == '\n' || c == '\r' then
        save(ls, '\n')
        inclinenumber(ls)
    elseif c == '\\' || c == '\"' || c == '\'' then
        save(ls, c)
        nextchar(ls)
    elseif c == END_OF_STREAM then
    else
        if !char_isdigit(c) then
            lex_error(ls, 'TK_string', "invalid escape sequence")
        end
        local bc = band(strbyte(c), 15) // Decimal escape '\ddd'.
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
            ls.p = ls.p + 1
            lex_error(ls, 'TK_string', "unfinished string")
        elseif c == '\n' || c == '\r' then
            lex_error(ls, 'TK_string', "unfinished string")
        elseif c == '\\' then
            read_escape_char(ls)
        else
            save_and_next(ls)
        end
    end
    save_and_next(ls) // skip delimiter
    return get_string(ls, 1, 1)
end

local function skip_line(ls)
    while !curr_is_newline(ls) && ls.current ~= END_OF_STREAM do
        savespace_and_next(ls)
    end
end

local single_chars = {
    ["?"] = Op.Cond,
    ["#"] = Op.Len,
    ["{"] = Token.LBrace,
    ["}"] = Token.RBrace,
    ["["] = Token.LBracket,
    ["]"] = Token.RBracket,
    ["("] = Token.LParens,
    [")"] = Token.RParens,
    [","] = Token.Comma,
    [";"] = Token.Semicolon,
    [END_OF_STREAM] = Token.EOF,
}

local eq_ops = {
    ["*"] = {Op.Mul, Op.MulAssign},
    ["%"] = {Op.Mod, Op.ModAssign},
    ["^"] = {Op.Exp, Op.ExpAssign},
    ["!"] = {Op.Not, Op.Ne},
    [">"] = {Op.Gt, Op.GtEq},
    ["<"] = {Op.Lt, Op.LtEq},
    ["*"] = {Op.Mul, Op.MulAssign},
    ["*"] = {Op.Mul, Op.MulAssign}
}

local function llex(ls)
    resetbuf(ls)
    while true do
        local current = ls.current
        if char_isident(current) then
            if char_isdigit(current) then // Numeric literal.
                return Literal.Number(lex_number(ls))
            end

            save_and_next(ls)
            local current = ls.current
            while (true) do
                if (!char_isident(current)) then
                    break
                end
                ls.save_buf = ls.save_buf .. current
                current = nextchar(ls)
            end

            local s = get_string(ls, 0, 0)
            local keyword = Keyword_KEYS[s]
            if keyword then
                s = keyword
            elseif s == "true" || s == "false" then
                s = Literal.Bool(s)
            elseif s == "nil" then
                s = Literal.Nil
            else
                s = Token.Ident(s)
            end
            return s, true
        end
        if current == '\n' || current == '\r' then
            inclinenumber(ls)
            continue
        elseif current == ' ' || current == '\t' || current == '\b' || current == '\f' then
            savespace_and_next(ls)
            continue
        elseif current == '=' then
            local next_char = check_nextchar(ls)
            if next_char == "=" then // ("==")
                nextchar(ls)
                return Op.Eq
            elseif next_char == ">" then
                nextchar(ls)
                return Token.Arrow // ("=>")
            end
            return Op.Assign // ("=")
        elseif current == "+" then
            local next_char = check_nextchar(ls)
            if next_char == "=" then // ("+=")
                nextchar(ls)
                return Op.AddAssign
            elseif next_char == "+" then // ("++")
                nextchar(ls)
                return Op.Incr
            end
            return Op.Add // ("+")
        elseif current == "-" then
            local next_char = check_nextchar(ls)
            if next_char == "=" then // ("-=")
                nextchar(ls)
                return Op.SubAssign
            elseif next_char == "-" then // ("--")
                nextchar(ls)
                return Op.Decr
            end
            return Op.Sub // ("-")
        elseif current == "/" then
            local next_char = check_nextchar(ls)
            if next_char == "=" then // ("/=")
                nextchar(ls)
                return Op.DivAssign
            elseif next_char == "/" then // ("Comment")
                skip_line(ls)
                continue
            elseif next_char == "*" then // ("Comment")
                read_long_comment(ls)
                continue
            end
            return Op.Div // ("/")
        elseif current == ':' then
            if check_nextchar(ls) == ":" then
                nextchar(ls)
                return Token.Label // ("::")
            end
            return Token.Colon // (":")
        elseif current == "." then
            if check_nextchar(ls) == '.' then
                nextchar(ls)
                if check_nextchar(ls) == "." then
                    nextchar(ls)
                    return Op.Ellipsis // ("...")
                end
                return Op.Concat // ("..")
            end
            return Op.Dot // (".")
        elseif current == "&" then
            if check_nextchar(ls) == "&" then
                nextchar(ls)
                return Op.LAnd
            end
        elseif current == "|" then
            if check_nextchar(ls) == "|" then
                nextchar(ls)
                return Op.LOr
            end
        elseif current == '"' then
            local str = read_string(ls, current)
            return Literal.String(str) // ("String")
        end
        local eq_op = eq_ops[current]
        if eq_op then
            if check_nextchar(ls) == "=" then
                nextchar(ls)
                return eq_op[2]
            end
            return eq_op[1] // (">")
        end
        local single_char = single_chars[current]
        if single_char then return single_char end
        lex_error(ls, nil, "unexpected character (" .. current .. ")")
    end
end

local Lexer = {
    token2str = token2str,
    error = lex_error,
}

function Lexer.next(ls)
    ls.lastline = ls.linenumber
    local token, skip_next = llex(ls)
    token.line = ls.linenumber
    ls.token = token
    ls.space = get_space_string(ls)
    if !skip_next then
        nextchar(ls)
    end
    return token
end

function Lexer.lookahead(ls)
    assert(ls.tklookahead == Token.EOF)
    ls.tklookahead, ls.tklookaheadval = llex(ls)
    ls.spaceahead = get_space_string(ls)
    return ls.tklookahead
end

local LexerClass = { __index = Lexer }

local setmetatable = setmetatable
local function lex_setup(chunkname, data, n)
    local ls = {
        linenumber = 1,
        lastline = 1,
        data = data,
        n = n,
        p = 1,
        chunkname = chunkname,
        space_buf = ''
    }
    nextchar(ls)
    return setmetatable(ls, LexerClass), ls:next()
end

return lex_setup