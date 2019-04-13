local operator = include("lau/operators.lua")

local LJ_52 = false

local parse_stmt, parse_func, parse_params, check_semicolon, parse_flags

local EndOfBlock = { TK_else = true, TK_elseif = true, TK_end = true, TK_until = true, TK_eof = true, ["}"] = true,
-- [";"] = true
}
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

local function lex_opt2(ls, tok, val)
    if ls.token == tok && ls.tokenval == val then
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
        local value = ls.tokenval
        local line = ls.linenumber
        ls:next()
        return value, line
    end
    err_token(ls, "TK_name")
end

local expr_primary, expr, expr_unop, expr_binop, expr_simple
local expr_list, expr_table, expr_array
local parse_body, parse_block, parse_args

local function var_lookup(ast, ls)
    local name, line = lex_str(ls)
    return ast:identifier(name, line)
end

local function expr_field(ast, ls, v)
    ls:next() -- Skip dot or colon.
    local key, line = lex_str(ls)
    return ast:expr_property(v, key, line)
end

local function expr_bracket(ast, ls)
    ls:next() -- Skip '['.
    local v = expr(ast, ls)
    lex_check(ls, "]")
    return v
end

-- ADDED PARSING METHODS
local function get_arrow_function(ast, ls, line, force, name) -- check if current token is arrow function and return parameters or return false
    local reset = ls:fake_llex()
    local args, vararg
    if !name then
        line = ls.lastline
        args, vararg = parse_params(ast, ls, nil, !force)
    else
        args, vararg = {ast:identifier(name, ls.lastline)}, false
    end
    if (args == false || ls.token != "TK_ar") then
        if (force) then
            err_token(ls, "TK_ar")
        end
        reset()
        return false
    else
        local return_line = ls.linenumber
        ls.fs.varargs = vararg
        ls:next()
        local body
        if (lex_opt(ls, "{")) then
            body = parse_block(ast, ls, line)
            lex_match(ls, "}", "TK_ar", line)
        else
            local exp = expr(ast, ls)
            body = {ast:return_stmt({exp}, return_line)}
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
    ls.skip_semicolon = nil
    local body
    if (lex_opt(ls, "{")) then
        body = parse_block(ast, ls, ls.lastline)
        lex_match(ls, "}", who, line)
    else
        ls.empty_statment = true
        line = ls.lastline
        local stmt, _ = parse_stmt(ast, ls)
        ls.empty_statment = nil
        body = {stmt}
        body = {ast:chunk(body, ls.chunkname, line, ls.lastline)}
    end
    ls.skip_semicolon = true
    return body
end

local function parse_async_func(ast, ls, line, islocal)
    ls:next()
    if (ls.token != "TK_function") then
        err_token(ls, "TK_function")
    end
    local func = parse_func(ast, ls, line)
    local proto = {
        varargs     = func.vararg,
        firstline   = func.firstline,
        lastline    = func.lastline
    }
    if (islocal) then
        return ast:async_local_function_decl(func.id, func.params, func.body, proto)
    else
        return ast:async_function_decl(func.id, func.params, func.body, proto, line)
    end
end

function expr_table(ast, ls)
    lex_check(ls, "{")
    local line = ls.lastline
    local kvs = {}
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
            key = ast:literal("goto", ls.linenumber)
            ls:next()
        elseif ls.token == '[' then
            key = expr_bracket(ast, ls)
        elseif (token == "TK_string" || token == "TK_number" || token == "TK_name") then
            key = ast:literal(ls.tokenval, ls.linenumber)
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
        if !lex_opt(ls, ",") then break end
    end
    lex_match(ls, "}", "{", line)
    return ast:expr_table(kvs, line, ls.lastline)
end

function expr_array(ast, ls)
    local line = ls.linenumber
    lex_check(ls, "[")
    local kvs = {}
    while ls.token != "]" do
        table.insert(kvs, {(expr(ast, ls))})
        if !lex_opt(ls, ',') then
            if (ls.token != "]") then
                err_token(ls, "}")
            end
            break
        end
    end
    lex_match(ls, "]", "[", line)
    return ast:expr_table(kvs, line, ls.lastline)
    -- return ast:expr_function_call(ast:identifier("newArray"), {ast:expr_table(kvs, line)}, line)
end

function expr_simple(ast, ls)
    local tk, val = ls.token, ls.tokenval
    local e
    if tk == "TK_number" then
        e = ast:literal(val, ls.linenumber)
    elseif tk == "TK_string" then
        e = ast:literal(val, ls.linenumber)
    elseif tk == "TK_nil" then
        e = ast:literal(nil, ls.linenumber)
    elseif tk == "TK_true" then
        e = ast:literal(true, ls.linenumber)
    elseif tk == "TK_false" then
        e = ast:literal(false, ls.linenumber)
    elseif tk == "TK_dots" then
        if !ls.fs.varargs then
            err_syntax(ls, "cannot use \"...\" outside a vararg function")
        end
        e = ast:expr_vararg(ls.linenumber)
    elseif tk == "[" then
        return expr_array(ast, ls)
    elseif tk == "{" then
        return expr_table(ast, ls)
    elseif tk == "TK_async" then
        local async_line, arg = ls.linenumber
        ls:next()
        local line = ls.linenumber
        if (ls.token == "(") then
            ls:next()
            local v = get_arrow_function(ast, ls, line, true)
            arg = ast:expr_function(v.args, v.body, v)
        elseif ls.token == 'TK_name' then
            local var = ls.tokenval
            ls:next()
            local v = get_arrow_function(ast, ls, line, true, var)
            arg = ast:expr_function(v.args, v.body, v)
        else
            lex_match(ls, "TK_function", "TK_async", ls.linenumber)
            arg = ast:expr_function(parse_body(ast, ls, line, false))
        end
        return ast:expr_async_function(arg, async_line, ls.lastline)
    elseif tk == "TK_function" then
        ls:next()
        local args, body, proto = parse_body(ast, ls, ls.linenumber, false)
        return ast:expr_function(args, body, proto)
    else
        local vk, v = expr_primary(ast, ls)
        if (v == "arrowFunction") then
            return ast:expr_function(vk.args, vk.body, vk, ls.linenumber)
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

local function parse_tenary(ast, ls, condition)
    ls:next()
    local old = ls.tenary_expression
    ls.tenary_expression = true
    local left = expr(ast, ls)
    ls.tenary_expression = old
    lex_check(ls, ':')
    return ast:tenary(condition, left, expr(ast, ls))
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
    if op == '?' && limit == 0 then
        v = parse_tenary(ast, ls, v)
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
        local args = get_arrow_function(ast, ls, line, false)
        if (args) then
            vk, v = "arrowFunction", args
        else
            vk, v = "expr", ast:expr_brackets(expr(ast, ls))
            lex_match(ls, ')', '(', line)
        end
    elseif ls.token == "TK_new" then
        local line = ls.linenumber
        ls:next()
        local exp = expr(ast, ls)
        if (exp.kind != "Identifier" && exp.kind != "CallExpression") then
            ls:error(nil, "expecting name")
        end
        vk, v = "call", ast:new_expr(exp)
    elseif ls.token == "TK_await" then
        local line = ls.linenumber
        ls:next()
        vk, v = "call", ast:await_expr(expr(ast, ls), line)
    elseif ls.token == "TK_name" then
        local line = ls.linenumber
        if ls:lookahead() == 'TK_ar' then
            local var = ls.tokenval
            ls:next()
            vk, v = "arrowFunction", get_arrow_function(ast, ls, line, false, var)
        else
            vk, v = "var", var_lookup(ast, ls)
        end
    elseif (ls.token == '++' || ls.token == '--') && ls.linenumber == (ls:lookahead() && ls.lookaheadline) then
        local line = ls.line
        local op = ls.token
        ls:next()
        ls.incrementing = true
        local _v, _vk = expr_primary(ast, ls)
        if _vk != 'indexed' && _vk != 'var' then
            err_token(ls, 'TK_name')
        end
        ls.incrementing = nil
        vk, v = 'expr', ast:increment_expr(_v, op, false, line)
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
            local line = ls.linenumber
            local key = expr_bracket(ast, ls)
            local lastline = ls.lastline
            vk, v = "indexed", ast:expr_index(v, key)
            v.firstline = line
            v.lastline = lastline
        elseif ls.token == ":" && !ls.tenary_expression then
            ls:next()
            local key = var_lookup(ast, ls)
            local args = parse_args(ast, ls)
            vk, v = "call", ast:expr_method_call(v, key, args, line, ls.lastline)
        elseif ls.token == "(" then
            local args = parse_args(ast, ls)
            vk, v = "call", ast:expr_function_call(v, args, ls.lastline)
        else
            break
        end
    end
    if vk == 'indexed' || vk == 'var' && !ls.incrementing && ls.linenumber == ls.lastline then
        local op = ls.token
        if (op == '++' || op == '--') then
            ls:next()
            vk, v = 'expr', ast:increment_expr(v, op, true)
        end
    end
    return v, vk
end

-- Parse 'return' statement.
local function parse_return(ast, ls, line)
    ls:next() -- Skip 'return'.
    ls.fs.has_return = true
    local exps
    if EndOfBlock[ls.token] || ls.token == ';' then -- Base return.
        exps = { }
    else -- Return with one or more values.
        exps = expr_list(ast, ls)
    end
    return ast:return_stmt(exps, line)
end

-- Parse numeric 'for'.
local function parse_for_num(ast, ls, varname, line, varline)
    lex_check(ls, '=')
    local init = expr(ast, ls)
    lex_check(ls, ',')
    local last = expr(ast, ls)
    local step
    if lex_opt(ls, ',') then
        step = expr(ast, ls)
    else
        step = ast:literal(1, ls.linenumber)
    end
    lex_check(ls, ")")
    local body = parseBodyOrTillSemicolon(ast, ls, line, "TK_for")
    local var = ast:identifier(varname, varline)
    return ast:for_stmt(var, init, last, step, body, line, ls.linenumber)
end

-- Parse 'for' iterator.
local function parse_for_iter(ast, ls, indexname, line, varline)
    local vars = { ast:identifier(indexname, varline) }
    while lex_opt(ls, ",") do
        vars[#vars + 1] = ast:identifier(lex_str(ls))
    end
    lex_check(ls, "TK_in")
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
    local varline = ls.lastline
    local stmt
    if ls.token == '=' then
        stmt = parse_for_num(ast, ls, varname, line, varline)
    elseif ls.token == "," || ls.token == "TK_in" then
        stmt = parse_for_iter(ast, ls, varname, line, varline)
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
        local token_line = ls.linenumber
        ls:next()
        args = { ast:literal(a, token_line) }
    else
        err_syntax(ls, "function arguments expected")
    end
    return args
end

local function parse_assignment(ast, ls, vlist, var, vk, local_decl)
    local line = ls.linenumber
    if (istable(var) && var.kind == "IncrementExpression") then
        var.kind = "IncrementStatement"
        return var
    end
    checkcond(ls, vk == "var" || vk == "indexed", "syntax error")
    vlist[#vlist + 1] = var
    if lex_opt(ls, ',') then
        local n_var, n_vk = expr_primary(ast, ls)
        return parse_assignment(ast, ls, vlist, n_var, n_vk, local_decl)
    else -- Parse RHS.
        local op = ls.token == 'TK_ao' && ls.tokenval || '='
        local assigning = true
        if lex_opt(ls, 'TK_ao') then
            if #vlist > 1 then
                err_syntax(ls, "can't assign multiple vars using '" .. op .. "='")
            else
                vlist = vlist[1]
            end
        elseif !lex_opt(ls, '=') then
            assigning = false
        end
        local exps
        if local_decl then
            if (assigning) then
                if (op != '=') then
                    exps = expr(ast, ls)
                else
                    exps = expr_list(ast, ls)
                end
            else
                exps = {}
            end
            return exps, op
        else
            if !assigning then
                lex_check(ls, '=')
            end
            if (op != '=') then
                exps = expr(ast, ls)
            else
                exps = expr_list(ast, ls)
            end
            return ast:assignment_expr(vlist, exps, op, line)
        end
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
        return (parse_async_func(ast, ls, line, true))
    elseif lex_opt(ls, 'TK_function') then -- Local function declaration.
        local name = var_lookup(ast, ls)
        local args, body, proto = parse_body(ast, ls, line, false)
        ls.skip_semicolon = true
        return ast:local_function_decl(name, args, body, proto)
    else -- Local variable declaration.
        local vl = {}
        local var = var_lookup(ast, ls)
        local exps, op = parse_assignment(ast, ls, vl, var, "var", true)
        if !op then
            return exps
        end
        return ast:local_decl(vl, exps, op, line)
    end
end

function parse_class(ast, ls, line)
    ls:next()
    local class_iden = var_lookup(ast, ls)
    lex_check(ls, "{")
    local parent = lex_opt(ls, 'TK_extends') && var_lookup(ast, ls)
    local constructor, fields, methods = nil, {}, {}
    while ls.token != "}" do
        local flags = parse_flags(ast, ls)
        local iden, line = lex_str(ls)
        if (ls.token == "(") then
            local method_iden = iden
            for _, v in ipairs(methods) do
                if (v.id.name == method_iden) then
                    ls:error(nil, "duplicate method declaration '" .. method_iden .. "' in class '" .. class_iden.name .. "'")
                end
            end
            local method
            do
                local args, body, proto = parse_body(ast, ls, line, !flags.static)
                local _iden = iden
                if (flags.static) then
                    local static_iden = ast:identifier("__class_statics", line)
                    _iden = ast:expr_property(static_iden, _iden, line)
                    local self_iden = ast:identifier("self", line)
                    local exp = ast:identifier(class_iden.name, line)
                    table.insert(body, 1,
                        ast:local_decl({self_iden}, {exp}, "=", line)
                    )
                end
                _iden = ast:expr_property(ast:identifier(class_iden.name, line), _iden, line)
                method = ast:function_decl(_iden, args, body, proto)
            end
            if (method_iden == class_iden.name) then
                if (constructor) then
                    ls.linenumber = line
                    ls:error(nil, "duplicate constructor definition for class '" .. method_iden .. "'")
                end
                if (flags.static) then
                    ls:error(nil, "constructor for '" .. method_iden .. "' is marked as 'static'")
                end
                constructor = method
            else
                table.insert(methods, method)
            end
        elseif ls.token == "=" then
            if (table.Count(flags) > 0 && !flags.static) then
                ls:error(nil, "unexpected identifier in class declaration")
            end
            local field = iden
            if (flags.static) then
                local static_iden = ast:identifier("__class_statics", line)
                field = ast:expr_property(static_iden, field, line)
            end
            field = ast:expr_property(ast:identifier(class_iden.name, line), field, line)
            local line = ls.linenumber
            lex_check(ls, "=")
            local exp = expr(ast, ls)
            local field_ast = ast:assignment_expr({field}, {exp}, "=", line)
            field_ast.static = flags.static
            table.insert(fields, field_ast)
            check_semicolon(ls, true)
        else
            ls:error(nil, "unexpected '" .. ls.token2str(ls.token) .. "' in class declaration")
        end
    end
    ls:next()
    ls.skip_semicolon = true
    return ast:class_decl(class_iden, parent, constructor, fields, methods)
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
    ls.skip_semicolon = true
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
    tests[#tests].firstline = line
    tests[#tests].bodystart = ls.lastline
    return parseBodyOrTillSemicolon(ast, ls, line, who)
end

local function parse_if(ast, ls, line)
    local tests, blocks = { }, { }
    blocks[1] = parse_then(ast, ls, tests, line, "TK_if")
    while ls.token == "TK_else" && ls:lookahead() == "TK_if" do
        local line = ls.linenumber
        ls:next()
        blocks[#blocks + 1] = parse_then(ast, ls, tests, line, "else if")
        tests[#blocks].firstline = line
    end
    local else_branch
    if ls.token == "TK_else" then
        local eline = ls.linenumber
        ls:next() -- Skip 'else'.
        else_branch = parseBodyOrTillSemicolon(ast, ls, eline, "TK_else")
        else_branch.line = eline
    end
    return ast:if_stmt(tests, blocks, else_branch, line, ls.lastline)
end

local function parse_label(ast, ls)
    ls:next() -- Skip '::'.
    local name = var_lookup(ast, ls)
    lex_check(ls, "TK_label")
    return ast:label_stmt(name, ls.linenumber)
end

local function parse_goto(ast, ls)
    local line = ls.lastline
    local name = var_lookup(ast, ls)
    return ast:goto_stmt(name, line)
end

function check_semicolon(ls, dont_skip)
    if ls.skip_semicolon && !dont_skip then
        ls.skip_semicolon = nil
    else
        if !lex_opt(ls, ';') then
            ls.linenumber = ls.lastline
            ls:error(ls.token, "';' expected to end statment")
        end
    end
end

-- Parse a statement. Returns the statement itself and a boolean that tells if it
-- must be the last one in a chunk.
function parse_flags(ast, ls)
    local flags = {}
    local flags_names = {"let", "static", "async"}
    while (table.HasValue(flags_names, ls.token2str(ls.token))) do
        local token = ls.token2str(ls.token)
        if (flags[token]) then
            ls:error(nil, "duplicate flag '", token, "'")
        else
            flags[token] = true
        end
        ls:next()
    end
    return flags
end

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
        lex_check(ls, "{")
        local body = parse_block(ast, ls)
        local lastline = ls.linenumber
        lex_match(ls, "}", "TK_do", line)
        ls.skip_semicolon = true
        stmt = ast:do_stmt(body, line, lastline)
    elseif token == "TK_for" then
        stmt = parse_for(ast, ls, line)
    elseif token == "TK_repeat" then
        stmt = parse_repeat(ast, ls, line)
    elseif token == "TK_async" then
        stmt = parse_async_func(ast, ls, line)
    elseif token == "TK_class" then
        stmt = parse_class(ast, ls, line)
    elseif token == "TK_function" then
        stmt = parse_func(ast, ls, line)
    elseif token == "TK_let" then
        ls:next()
        stmt = parse_local(ast, ls, line)
    elseif token == "TK_return" then
        stmt = parse_return(ast, ls, line)
        check_semicolon(ls, true)
        return stmt, true -- Must be last.
    elseif token == "TK_break" then
        ls:next()
        stmt = ast:break_stmt(line)
        check_semicolon(ls, true)
        return stmt, !LJ_52 -- Must be last in Lua 5.1.
    elseif token == "TK_continue" then
        ls:next()
        stmt = ast:continue_stmt(line)
        check_semicolon(ls, true)
        return stmt, true
    elseif token == "TK_label" then
        stmt = parse_label(ast, ls)
    elseif token == "TK_goto" then
        if ls:lookahead() == "TK_name" then
            ls:next()
            stmt = parse_goto(ast, ls)
        end
    end

    if (!stmt && ls.empty_statment && lex_opt(ls, ";")) then
        return
    end
    -- If here 'stmt' is "nil" then ls.token didn't match any of the previous rules.
    -- Fall back to call/assign rule.
    if !stmt then
        stmt = parse_call_assign(ast, ls)
    end
    if (stmt) then
        check_semicolon(ls)
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
        args[1] = ast:identifier("self", ls.linenumber)
    end
    if ls.token != ")" then
        repeat
            local token = ls.token
            if token == "TK_name" || (!LJ_52 && token == "TK_goto") then
                local name = var_lookup(ast, ls)
                if (lex_opt(ls, '=')) then
                    name = {name = name, default_value = expr(ast, ls)}
                end
                args[#args + 1] = name
            elseif token == "TK_dots" then
                ls:next()
                vararg = ls.lastline
                break
            else
                if (no_check) then
                    return false
                end
                err_syntax(ls, "<name> or \"...\" expected")
            end
        until !lex_opt(ls, ',')
    end
    if ls.token != ')' then
        if (no_check) then
            return false
        end
        err_token(ls, tok)
    end
    ls:next()
    return args, vararg
end

local function new_proto(ls, varargs)
    return { varargs = varargs }
end

local function parse_block_stmts(ast, ls)
    local firstline = ls.linenumber
    local stmt, islast, lastline = nil, false
    local body = { }
    while !islast && !EndOfBlock[ls.token] do
        stmt, islast, lastline = parse_stmt(ast, ls)
        body[#body + 1] = stmt
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