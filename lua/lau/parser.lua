local operator = include("lau/operators.lua")

local LJ_52 = false

local parse_stmt, parse_func, parse_params

local EndOfBlock = { TK_else = true, TK_elseif = true, TK_end = true, TK_until = true, TK_eof = true, ["}"] = true, [";"] = true}
local function err_syntax(ls, em)
    ls:error(ls.token, em)
end

local function err_token(ls, token)
    -- PrintType(ls)
    ls:error(ls.token, "'%s' expected", ls.token2str(token))
end

local function checkcond(ls, cond, em)
    if !cond then err_syntax(ls, em) end
end

local function lex_opt(ls, tok)
    if ls.token == tok then
        ls:next()
        return true
    end
    return false
end

local function lex_check(ls, tok)
    if ls.token != tok then err_token(ls, tok) end
    ls:next()
end

local function lex_match(ls, what, who, line)
    if !lex_opt(ls, what) then
        if line == ls.linenumber then
            err_token(ls, what)
        else
            local token2str = ls.token2str
            ls:error(ls.token, "'%s' expected (to close '%s' at line %d)", token2str(what), token2str(who), line)
        end
    end
end

local function lex_str(ls)
    if ls.token == "TK_goto" then
        return "goto", ls:next()
    elseif ls.token == "TK_name" then
        return ls.tokenval, ls:next()
    end
    err_token(ls, "TK_name")
end

local expr_primary, expr, expr_unop, expr_binop, expr_simple
local expr_list, expr_table, expr_array
local parse_body, parse_block, parse_args

local function var_lookup(ast, ls)
    local name = lex_str(ls)
    return ast:identifier(name)
end

local function expr_field(ast, ls, v)
    ls:next() -- Skip dot or colon.
    local key = lex_str(ls)
    return ast:expr_property(v, key)
end

local function expr_bracket(ast, ls)
    ls:next() -- Skip '['.
    local v = expr(ast, ls)
    lex_check(ls, "]")
    return v
end

-- ADDED PARSING METHODS
local function get_arrow_function(ast, ls, line, force) -- check if current token is arrow function and return parameters or return false
    local reset = ls:fake_llex()
    local args, vararg = parse_params(ast, ls, nil, !force)
    if (args == false || ls.token != "TK_ar") then
        if (force) then
            err_token(ls, "TK_ar")
        end
        reset()
        return false
    else
        ls:next()
        local body
        if (lex_opt(ls, "{")) then
            body = parse_block(ast, ls, line)
            lex_match(ls, "}", "TK_ar", line)
        else
            body = {ast:return_stmt({(expr(ast, ls))}, line)}
            if (!ls.IsInFunctionCall && !ls.IsInTable && ls.previousToken != ";" && ls.token != "TK_eof" && ls.space:match("\n") == nil) then
                lex_check(ls, ";")
            end
        end
        return {body = body, args = ast:func_parameters_decl(args, vararg), firstline = line, lastline = ls.linenumber}
    end
end

local function parse_functionMember(ast, ls, line)
    return ast:expr_function(parse_body(ast, ls, ls.linenumber, false))
end

local function parse_PrefixExpressionInParens(ast, ls)
    lex_check(ls, "(")
    return ast:expr_brackets(expr(ast, ls)), lex_check(ls, ")")
end

local function parseBodyOrTillSemicolon(ast, ls, line, who)
    if (lex_opt(ls, "{")) then
        return parse_block(ast, ls, line), lex_match(ls, "}", who, line)
    else
        local stmt, _ = parse_stmt(ast, ls)
        local body = {stmt}
        body = ast:chunk(body, ls.chunkname, 0, 3)
        return {body}, (ls.previousToken != ";" && ls.token != "TK_eof" && ls.space:match("\n") == nil) && lex_check(ls, ";")
    end
end

local function parse_async_func(ast, ls, line, islocal)
    ls:next()
    if (ls.token != "TK_function") then
        err_token(ls, "TK_function")
    end
    local func = parse_func(ast, ls, line)
    local func_expr = ast:expr_function(func.params, func.body, {
        varargs     = func.vararg,
        firstline   = func.firstline,
        lastline    = func.lastline
    })
    if (islocal) then
        return ast:local_decl({func.id.name}, {ast:expr_async_function(func_expr, line)}, line)
    else
        return ast:assignment_expr({func.id}, {ast:expr_async_function(func_expr, line)}, line)
    end
end

function expr_table(ast, ls)
    lex_check(ls, "{")
    local line = ls.linenumber
    local kvs = {}
    ls.IsInTable = true
    while ls.token != "}" do
        local key
        local token = ls.token

        local isAsync
        if (token == "TK_async") then
            ls:next()
            isAsync = true
            token = ls.token
        end

        if (token == "TK_goto") then
            key = ast:literal("goto")
            ls:next()
        elseif (token == "TK_string" || token == "TK_number" || token == "TK_name") then
            key = ast:literal(ls.tokenval)
            ls:next()
        else
            err_token(ls, "TK_name")
        end

        local val
        if (isAsync) then
            val = parse_functionMember(ast, ls, line)
            val = ast:expr_async_function(val, line)
        elseif (ls.token == "(") then
            val = parse_functionMember(ast, ls, line)
        else
            lex_check(ls, ":")
            val = expr(ast, ls)
        end

        table.insert(kvs, {val, key}) -- "key" can be nil.
        if !lex_opt(ls, ",") && !lex_opt(ls, ';') then break end
    end
    ls.IsInTable = nil
    lex_match(ls, "}", "{", line)
    return ast:expr_table(kvs, line)
end

function expr_array(ast, ls)
    lex_check(ls, "[")
    local line = ls.linenumber
    local kvs = {}
    while ls.token != "]" do
        table.insert(kvs, {(expr(ast, ls))})
        if !lex_opt(ls, ',') && !lex_opt(ls, ';') then
            if (ls.token != "]") then
                err_token(ls, "}")
            end
            break
        end
    end
    lex_match(ls, "]", "[", line)
    return ast:expr_function_call(ast:identifier("newArray"), {ast:expr_table(kvs, line)}, line)
end

function expr_simple(ast, ls)
    local tk, val = ls.token, ls.tokenval
    local e
    if tk == "TK_number" then
        e = ast:literal(val)
    elseif tk == "TK_string" then
        e = ast:literal(val)
    elseif tk == "TK_nil" then
        e = ast:literal(nil)
    elseif tk == "TK_true" then
        e = ast:literal(true)
    elseif tk == "TK_false" then
        e = ast:literal(false)
    elseif tk == "TK_dots" then
        if !ls.fs.varargs then
            err_syntax(ls, "cannot use \"...\" outside a vararg function")
        end
        e = ast:expr_vararg()
    elseif tk == "[" then
        return expr_array(ast, ls)
    elseif tk == "{" then
        return expr_table(ast, ls)
    elseif tk == "TK_async" then
        ls:next()
        local line, arg
        if (ls.token == "(") then
            line = ls.linenumber
            ls:next()
            local v = get_arrow_function(ast, ls, line, true)
            arg = ast:expr_function(v.args, v.body, v)
        else
            lex_match(ls, "TK_function", "TK_async", ls.linenumber)
            line = ls.linenumber
            arg = ast:expr_function(parse_body(ast, ls, line, false))
        end
        return ast:expr_async_function(arg, line)
    elseif tk == "TK_function" then
        ls:next()
        local args, body, proto = parse_body(ast, ls, ls.linenumber, false)
        return ast:expr_function(args, body, proto)
    else
        local vk, v = expr_primary(ast, ls)
        if (v == "arrowFunction") then
            return ast:expr_function(vk.args, vk.body, vk)
        end
        return vk, v
    end
    ls:next()
    return e
end

function expr_list(ast, ls)
    local exps = { }
    exps[1] = expr(ast, ls)
    while lex_opt(ls, ",") do
        exps[#exps + 1] = expr(ast, ls)
    end
    local n = #exps
    if n > 0 then
        exps[n] = ast:set_expr_last(exps[n])
    end
    return exps
end

function expr_unop(ast, ls)
    local tk = ls.token
    if tk == "TK_not" || tk == "TK_!" || tk == "-" || tk == "#" then
        local line = ls.linenumber
        ls:next()
        local v = expr_binop(ast, ls, operator.unary_priority)
        return ast:expr_unop(ls.token2str(tk), v, line)
    else
        return expr_simple(ast, ls)
    end
end

-- Parse binary expressions with priority higher than the limit.
function expr_binop(ast, ls, limit)
    local v = expr_unop(ast, ls)
    local op = ls.token2str(ls.token)
    while operator.is_binop(op) && operator.left_priority(op) > limit do
        local line = ls.linenumber
        ls:next()
        local v2, nextop = expr_binop(ast, ls, operator.right_priority(op))
        v = ast:expr_binop(op, v, v2, line)
        op = nextop
    end
    return v, op
end

function expr(ast, ls)
    return expr_binop(ast, ls, 0) -- Priority 0: parse whole expression.
end

-- Parse primary expression.
function expr_primary(ast, ls)
    local v, vk
    -- Parse prefix expression.
    if ls.token == "(" then
        local line = ls.linenumber
        ls:next()
        local args = get_arrow_function(ast, ls, line)
        if (args) then
            vk, v = "arrowFunction", args
        else
            vk, v = "expr", ast:expr_brackets(expr(ast, ls))
            lex_match(ls, ')', '(', line)
        end
    elseif ls.token == "TK_name" then
        vk, v = "var", var_lookup(ast, ls)
    elseif ls.token == "TK_goto" then
        vk, v = "var", var_lookup(ast, ls)
    else
        err_syntax(ls, "unexpected symbol")
    end
    while true do -- Parse multiple expression suffixes.
        local line = ls.linenumber
        if ls.token == "." then
            vk, v = "indexed", expr_field(ast, ls, v)
        elseif ls.token == "[" then
            local key = expr_bracket(ast, ls)
            vk, v = "indexed", ast:expr_index(v, key)
        elseif ls.token == ":" then
            ls:next()
            local key = lex_str(ls)
            local args = parse_args(ast, ls)
            vk, v = "call", ast:expr_method_call(v, key, args, line)
        elseif ls.token == "(" || ls.token == "TK_string" || ls.token == "{" then
            ls.IsInFunctionCall = true
            local args = parse_args(ast, ls)
            ls.IsInFunctionCall = nil
            vk, v = "call", ast:expr_function_call(v, args, line)
        else
            break
        end
    end
    return v, vk
end

-- Parse 'return' statement.
local function parse_return(ast, ls, line)
    ls:next() -- Skip 'return'.
    ls.fs.has_return = true
    local exps
    if EndOfBlock[ls.token] || ls.token == ";" then -- Base return.
        exps = { }
    else -- Return with one or more values.
        exps = expr_list(ast, ls)
    end
    return ast:return_stmt(exps, line)
end

-- Parse numeric 'for'.
local function parse_for_num(ast, ls, varname, line)
    lex_check(ls, '=')
    local init = expr(ast, ls)
    lex_check(ls, ';')
    local last = expr(ast, ls)
    lex_check(ls, ';')
    local step = expr(ast, ls)
    lex_check(ls, ")")
    local body = parseBodyOrTillSemicolon(ast, ls, line, "TK_for")
    local var = ast:identifier(varname)
    return ast:for_stmt(var, init, last, step, body, line, ls.linenumber)
end

-- Parse 'for' iterator.
local function parse_for_iter(ast, ls, indexname)
    local vars = { ast:identifier(indexname) }
    while lex_opt(ls, ",") do
        vars[#vars + 1] = ast:identifier(lex_str(ls))
    end
    lex_check(ls, "TK_in")
    local line = ls.linenumber
    local exps = expr_list(ast, ls)
    lex_check(ls, ")")
    local body = parseBodyOrTillSemicolon(ast, ls, line, "TK_for")
    return ast:for_iter_stmt(vars, exps, body, line, ls.linenumber)
end

-- Parse 'for' statement.
local function parse_for(ast, ls, line)
    ls:next()  -- Skip 'for'.
    lex_check(ls, "(")
    local varname = lex_str(ls)  -- Get first variable name.
    local stmt
    if ls.token == "=" then
        stmt = parse_for_num(ast, ls, varname, line)
    elseif ls.token == "," || ls.token == "TK_in" then
        stmt = parse_for_iter(ast, ls, varname)
    else
        err_syntax(ls, "'=' or 'in' expected")
    end
    return stmt
end

local function parse_repeat(ast, ls, line)
    ast:fscope_begin()
    ls:next() -- Skip 'repeat'.
    local body = parse_block(ast, ls)
    local lastline = ls.linenumber
    lex_match(ls, "TK_until", "TK_repeat", line)
    local cond = expr(ast, ls) -- Parse condition.
    ast:fscope_end()
    return ast:repeat_stmt(cond, body, line, lastline)
end

-- Parse function argument list.
function parse_args(ast, ls)
    local line = ls.linenumber
    local args
    if ls.token == "(" then
        if !LJ_52 && line != ls.lastline then
            err_syntax(ls, "ambiguous syntax (function call x new statement)")
        end
        ls:next()
        if ls.token != ")" then -- Not f().
            args = expr_list(ast, ls)
        else
            args = { }
        end
        lex_match(ls, ")", "(", line)
    elseif ls.token == "{" then
        local a = expr_table(ast, ls)
        args = { a }
    elseif ls.token == "TK_string" then
        local a = ls.tokenval
        ls:next()
        args = { ast:literal(a) }
    else
        err_syntax(ls, "function arguments expected")
    end
    return args
end

local function parse_assignment(ast, ls, vlist, var, vk)
    local line = ls.linenumber
    checkcond(ls, vk == "var" || vk == "indexed", "syntax error")
    vlist[#vlist+1] = var
    if lex_opt(ls, ',') then
        local n_var, n_vk = expr_primary(ast, ls)
        return parse_assignment(ast, ls, vlist, n_var, n_vk)
    else -- Parse RHS.
        lex_check(ls, '=')
        local exps = expr_list(ast, ls)
        return ast:assignment_expr(vlist, exps, line)
    end
end

local function parse_call_assign(ast, ls)
    local var, vk = expr_primary(ast, ls)
    if vk == "call" then
        return ast:new_statement_expr(var, ls.linenumber)
    else
        local vlist = { }
        return parse_assignment(ast, ls, vlist, var, vk)
    end
end

-- Parse 'local' statement.
local function parse_local(ast, ls)
    local line = ls.linenumber
    if ls.token == "TK_async" then
        return ( parse_async_func(ast, ls, line, true))
    elseif lex_opt(ls, 'TK_function') then -- Local function declaration.
        local name = lex_str(ls)
        local args, body, proto = parse_body(ast, ls, line, false)
        return ast:local_function_decl(name, args, body, proto)
    else -- Local variable declaration.
        local vl = { }
        repeat -- Collect LHS.
            vl[#vl+1] = lex_str(ls)
        until !lex_opt(ls, ',')
        local exps
        if lex_opt(ls, '=') then -- Optional RHS.
            exps = expr_list(ast, ls)
        else
            exps = { }
        end
        return ast:local_decl(vl, exps, line)
    end
end

function parse_func(ast, ls, line)
    local needself = false
    ls:next() -- Skip 'function'.
    -- Parse function name.
    local v = var_lookup(ast, ls)
    while ls.token == '.' do -- Multiple dot-separated fields.
        v = expr_field(ast, ls, v)
    end
    if ls.token == ':' then -- Optional colon to signify method call.
        needself = true
        v = expr_field(ast, ls, v)
    end
    local args, body, proto = parse_body(ast, ls, line, needself)
    return ast:function_decl(v, args, body, proto)
end

local function parse_while(ast, ls, line)
    ls:next() -- Skip 'while'.
    local cond = parse_PrefixExpressionInParens(ast, ls)
    ast:fscope_begin()
    local body = parseBodyOrTillSemicolon(ast, ls, line, "TK_while")
    local lastline = ls.linenumber
    ast:fscope_end()
    return ast:while_stmt(cond, body, line, lastline)
end

local function parse_then(ast, ls, tests, line, who)
    ls:next()
    table.insert(tests, parse_PrefixExpressionInParens(ast, ls))
    return parseBodyOrTillSemicolon(ast, ls, line, who)
end

local function parse_if(ast, ls, line)
    local tests, blocks = { }, { }
    blocks[1] = parse_then(ast, ls, tests, line, "TK_if")
    while ls.token == "TK_else" && ls:lookahead() == "TK_if" do
        ls:next()
        blocks[#blocks+1] = parse_then(ast, ls, tests, ls.linenumber, "else if")
    end
    local else_branch
    if ls.token == "TK_else" then
        local eline = ls.linenumber
        ls:next() -- Skip 'else'.
        else_branch = parseBodyOrTillSemicolon(ast, ls, eline, "TK_else")
    end
    return ast:if_stmt(tests, blocks, else_branch, line)
end

local function parse_label(ast, ls)
    ls:next() -- Skip '::'.
    local name = lex_str(ls)
    lex_check(ls, "TK_label")
    -- Recursively parse trailing statements: labels and ';' (Lua 5.2 only).
    while true do
        if ls.token == "TK_label" then
            parse_label(ast, ls)
        elseif ls.token == ';' then
            ls:next()
        else
            break
        end
    end
    return ast:label_stmt(name, ls.linenumber)
end

local function parse_goto(ast, ls)
    local line = ls.linenumber
    local name = lex_str(ls)
    return ast:goto_stmt(name, line)
end

-- Parse a statement. Returns the statement itself and a boolean that tells if it
-- must be the last one in a chunk.
function parse_stmt(ast, ls)
    local token = ls.token
    local line = ls.linenumber
    local stmt
    if token == "TK_if" then
        stmt = parse_if(ast, ls, line)
    elseif token == "TK_while" then
        stmt = parse_while(ast, ls, line)
    elseif token == "TK_do" then
        ls:next()
        local body = parse_block(ast, ls)
        local lastline = ls.linenumber
        lex_match(ls, "TK_end", "TK_do", line)
        stmt = ast:do_stmt(body, line, lastline)
    elseif token == "TK_for" then
        stmt = parse_for(ast, ls, line)
    elseif token == "TK_repeat" then
        stmt = parse_repeat(ast, ls, line)
    elseif token == "TK_async" then
        stmt = parse_async_func(ast, ls, line)
    elseif token == "TK_function" then
        stmt = parse_func(ast, ls, line)
    elseif token == "TK_let" then
        ls:next()
        stmt = parse_local(ast, ls, line)
    elseif token == "TK_return" then
        stmt = parse_return(ast, ls, line)
        return stmt, true -- Must be last.
    elseif token == "TK_break" then
        ls:next()
        stmt = ast:break_stmt(line)
        return stmt, !LJ_52 -- Must be last in Lua 5.1.
    elseif token == "TK_continue" then
        ls:next()
        stmt = ast:continue_stmt(line)
        return stmt, true
    elseif token == "TK_label" then
        stmt = parse_label(ast, ls)
    elseif token == "TK_goto" then
        if ls:lookahead() == "TK_name" then
            ls:next()
            stmt = parse_goto(ast, ls)
        end
    end

    if (!stmt && lex_opt(ls, ";")) then return end
    -- If here 'stmt' is "nil" then ls.token didn't match any of the previous rules.
    -- Fall back to call/assign rule.
    if !stmt then
        stmt = parse_call_assign(ast, ls)
    end
    return stmt, false
end

function parse_params(ast, ls, needself, no_check)
    if (no_check == nil) then
        lex_check(ls, "(")
    end
    local args = { }
    local vararg = false
    if needself then
        args[1] = "self"
    end
    if ls.token != ")" then
        repeat
            local token = ls.token
            if token == "TK_name" || (!LJ_52 && token == "TK_goto") then
                local name = lex_str(ls)
                if (lex_opt(ls, '=')) then
                    name = {name = name, default_value = expr(ast, ls)}
                end
                args[#args+1] = name
            elseif token == "TK_dots" then
                ls:next()
                vararg = true
                break
            else
                if (no_check) then
                    return false
                else
                    err_syntax(ls, "<name> or \"...\" expected")
                end
            end
        until !lex_opt(ls, ',')
    end
    lex_check(ls, ")")
    return args, vararg
end

local function get_after_params(ast, ls)
    local args = { }
    local vararg = false
    local token = ls.token
    if token != ")" then
        repeat
            if ls.token == "TK_name" || (!LJ_52 && ls.token == "TK_goto") then
                local name = lex_str(ls)
                args[#args+1] = name
            elseif ls.token == "TK_dots" then
                ls:next()
                token = llex(ls)
                vararg = true
                break
            else
                err_syntax(ls, "<name> or \"...\" expected")
            end
        until !lex_opt(ls, ',')
    end
    lex_check(ls, ")")
    return args, vararg
end

local function new_proto(ls, varargs)
    return { varargs = varargs }
end

local function parse_block_stmts(ast, ls)
    local firstline = ls.linenumber
    local stmt, islast = nil, false
    local body = { }
    while !islast && !EndOfBlock[ls.token] do
        stmt, islast = parse_stmt(ast, ls)
        body[#body + 1] = stmt
        lex_opt(ls, ';')
    end
    return body, firstline, ls.linenumber
end

local function parse_chunk(ast, ls)
    local body, _, lastline = parse_block_stmts(ast, ls)
    return ast:chunk(body, ls.chunkname, 0, lastline)
end

-- Parse body of a function.
function parse_body(ast, ls, line, needself)
    local pfs = ls.fs
    ls.fs = new_proto(ls, false)
    ast:fscope_begin()
    ls.fs.firstline = line
    local args, vararg = parse_params(ast, ls, needself)
    local params = ast:func_parameters_decl(args, vararg)
    ls.fs.varargs = vararg
    lex_check(ls, "{")
    local body = parse_block(ast, ls)
    ast:fscope_end()
    local proto = ls.fs
    if ls.token != '}' then
        lex_match(ls, '}', 'TK_function', line)
    end
    ls.fs.lastline = ls.linenumber
    ls:next()
    ls.fs = pfs
    return params, body, proto
end

function parse_block(ast, ls, firstline)
    ast:fscope_begin()
    local body = parse_block_stmts(ast, ls)
    body.firstline, body.lastline = firstline, ls.linenumber
    ast:fscope_end()
    return body
end

local function parse(ast, ls)
    ls:next()
    ls.fs = new_proto(ls, true)
    ast:fscope_begin()
    local chunk = parse_chunk(ast, ls)
    ast:fscope_end()
    if ls.token != 'TK_eof' then
        err_token(ls, 'TK_eof')
    end
    return chunk
end

return parse