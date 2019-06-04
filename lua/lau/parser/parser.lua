local operator = include("lau/parser/operator.lua")
local ast        = include("lau/ast.lua")

local format = string.format

local lexer = Lau.lexer
local Keyword = lexer.Keyword
local Literal = lexer.Literal
local Op = lexer.Op
local Token = lexer.Token

local function is_literal(v)
    return Literal[v.key] and true or false
end

local consume, expect, next_is, next_is_in, peek

local parse, parse_args, parse_body, parse_block, parse_assignment,
    parse_call_assign, parse_params, parse_ident

local parse_expr, parse_expr_list, parse_binop_expr, parse_unop_expr, parse_primary_expr,
    parse_simple_expr, parse_field_expr, parse_bracket_expr,
    parse_table_expr, parse_arrow_func_expr

local parse_stmt, parse_if_stmt, parse_do_stmt, parse_while_stmt,
    parse_for_stmt, parse_foreach_stmt, parse_fornum_stmt,
    parse_let_stmt, parse_return_stmt, parse_break_stmt,
    parse_continue_stmt, parse_label_stmt, parse_goto_stmt,
    parse_func_stmt, parse_class_stmt, parse_use_stmt

local Self
function parse(s)
    Self = s
    Self.varargs = true

    local tree = {}
    local stmt, is_last = nil, false

    while not is_last and Self.token ~= Token.EOF do
        stmt, is_last = parse_stmt()
        tree[#tree + 1] = stmt
    end

    if Self.token ~= Token.EOF then
        expect(Token.EOF)
    end

    return ast.chunk(tree, Self.chunkname)
end

function consume(value)
    local token = Self.token

    if token == value then
        Self:next()
        return token
    end

    return false
end

function expect(expected)
    if Self.token == expected then
        Self:next()
        return true
    else
        Self:error(format("expected %s", expected), Self.lastline)
    end
end

function next_is(expected)
    local token = Self.token

    return token == expected
end

function next_is_in(possibilities)
    local token = Self.token

    for i = 1, #possibilities do
        local v = possibilities[i]

        if v == Literal then
            if is_literal(token) then
                return true
            end
        elseif v == token then
            return true
        end
    end

    return false
end

function peek()
    local token = Self.token

    -- print("\n----------------")
    -- PrintType(debug.getinfo(2))

    if token == Token.EOF then
        Self:error("unexpected EOF")
    else
        return token
    end
end

--[[
    Expressions
]]

function parse_expr()
    return parse_binop_expr(0)
end

function parse_expr_list()
    local exps = {(
        parse_expr()
    )}

    while consume(Token.Comma) do
        exps[#exps + 1] = parse_expr()
    end

    return exps
end

local is_binop = operator.is_binop
local left_priority = operator.left_priority
local right_priority = operator.right_priority
function parse_binop_expr(limit)
    local v  = parse_unop_expr()
    local op = Self.token

    while is_binop(op) and left_priority(op) > limit do
        Self:next()

        local v2, nextop = parse_binop_expr(right_priority(op))
        v = ast.expr_binop(op, v, v2)
        op = nextop
    end

    return v, op
end

function parse_unop_expr()
    local tk = Self.token

    if tk == Op.Not or tk == Op.Sub or tk == Op.Len then
        Self:next()
        local v = parse_binop_expr(operator.unary_priority)

        return ast.expr_unop(tk, v)
    else
        return parse_simple_expr()
    end
end

local async_error_msg = "expected 'function' for async keyword"
function parse_primary_expr(is_async)
    local v, vk

    if next_is(Token.LParens) then
        local arrow_func = parse_arrow_func_expr(nil, nil, is_async)
        if arrow_func then
            return arrow_func
        end

        if is_async then Self:error(async_error_msg) end

        Self:next()

        local expr = parse_expr()
        expect(Token.RParens)
        vk, v = "expr", ast.expr_parentheses(expr)
    elseif next_is(Token.Ident) then
        vk, v = "var", parse_ident()

        if next_is(Token.Arrow) then
            return parse_arrow_func_expr(v, nil, is_async)
        end

        if is_async then Self:error(async_error_msg, Self.lastline) end
    else
        if is_async then Self:error(async_error_msg) end
        Self:error(
            format("expected expression near %s", peek())
        )
    end

    ::REPEAT::

    local token = Self.token

    if token == Op.Dot then
        Self:next()

        if not next_is_in{Literal, Token.Ident} then
            Self:error(
                format("unexpected %s, expected <key>", peek())
            )
        end

        if next_is(Literal.Nil) then
            Self:error("can't index a table with 'nil'")
        end

        local key = peek()
        Self:next()

        if key == Token.Ident then
            v = ast.expr_property(v, ast.identifier(key))
        else
            v = ast.expr_index(v, ast.literal(key))
        end

        vk = "indexed"
    elseif token == Token.LBracket then
        local key = parse_bracket_expr()
        vk, v = "indexed", ast.expr_index(v, key)
    elseif token == Token.Colon then
        Self:next()

        local key = parse_ident()
        local args = parse_args()
        vk, v = "call", ast.expr_method_call(v, key, args)
    elseif token ==  Token.LParens then
        local args = parse_args()
        vk, v = "call", ast.expr_function_call(v, args)
    else
        goto BREAK
    end

    goto REPEAT

    ::BREAK::

    return v, vk
end

function parse_simple_expr()
    local token = Self.token
    if not token then
        Self:error("expected expression", Self.lastline)
    end

    local is_async
    if token == Keyword.Async then
        is_async = true
        token = Self:next()
    end

    if is_literal(token) then
        if is_async then Self:error(async_error_msg) end

        Self:next()

        return ast.literal(token)
    elseif token == Op.Ellipsis then
        if Self:next() == Token.Arrow then
            return parse_arrow_func_expr(ast.expr_vararg(), true, is_async)
        end

        if is_async then Self:error(async_error_msg) end

        if not Self.varargs then
            Self:error("cannot use '...' outside a vararg function", Self.lastline)
        end

        return ast.expr_vararg()
    elseif token == Token.LBrace then
        if is_async then Self:error(async_error_msg) end

        return parse_table_expr()
    else
        return parse_primary_expr(is_async)
    end

    Self:next()

    return e
end

function parse_field_expr(v)
    Self:next()

    local key = parse_ident()

    return ast.expr_property(v, key)
end

function parse_bracket_expr()
    Self:next()

    local v = parse_expr()
    expect(Token.RBracket)

    return v
end

function parse_table_expr()
    expect(Token.LBrace)

    local kvs = {}

    while not next_is(Token.RBrace) do
        local key, val

        local token = Self.token
        if token == Token.LBracket then
            if Self:lookahead() == Literal.Nil then
                Self:error("can't use 'nil' as a key in tables")
            end

            key = parse_bracket_expr()
            key.bracketed = true

            expect(Token.Colon)
        elseif is_literal(token) or token == Token.Ident then
            if Self:lookahead() == Token.Colon then
                if token == Literal.Nil then
                    Self:error("can't use 'nil' as a key in tables")
                end

                Self:next()
                Self:next()

                key = ast.literal(token)
            end
        end

        if not val then
            val = parse_expr()
        end

        kvs[#kvs + 1] = {key, val}

        if not consume(Token.Comma) then
            break
        end
    end

    expect(Token.RBrace)

    return ast.expr_table(kvs)
end

function parse_arrow_func_expr(name, vararg, is_async)
    local reset = Self:fake_llex()
    local params

    local old_varargs = Self.varargs

    if vararg then
        Self.varargs = true
    else
        Self.varargs = false
    end

    if not name then
        local status
        status, params = pcall(parse_params)

        if status == false then
            params = false
        end
    else
        params = {name}
    end

    if not name and (params == false or not next_is(Token.Arrow)) then
        reset()

        return false
    else
        local token, body = Self:next()

        if token == Token.LBrace then
            body = parse_block()
        else
            local expr = parse_expr()
            body = {
                ast.return_stmt({
                    expr
                })
            }
        end

        Self.varargs = old_varargs

        return ast.expr_function(body, params, is_async)
    end
end

--[[
    Statements
]]

local statements
local main_statements
function parse_stmt()
    local is_let   = consume(Keyword.Let)
    local is_async = consume(Keyword.Async)

    if not statements then
        statements = {
            [Keyword.If] = parse_if_stmt,
            [Keyword.Do] = parse_do_stmt,
            [Keyword.While] = parse_while_stmt,
            [Keyword.For] = parse_for_stmt,
            [Keyword.Return] = parse_return_stmt,
            [Keyword.Break] = parse_break_stmt,
            [Keyword.Continue] = parse_continue_stmt,
            [Token.Label] = parse_label_stmt,
            [Keyword.Goto] = parse_goto_stmt,
            [Keyword.Fn] = parse_func_stmt,
            [Keyword.Class] = parse_class_stmt,
            [Keyword.Use] = parse_use_stmt,
            [Keyword.Enum] = function()
                Self:error("Enum is not implemented yet")
            end
        }

        main_statements = {Keyword.Fn, Keyword.Class, Keyword.Enum}
    end

    local token = Self.token
    local statement = statements[token]

    if is_async and token ~= Keyword.Fn then
        expect(Keyword.Fn)
    end

    if is_let then
        if next_is_in(main_statements) then
            return statement(true, is_async)
        else
            return parse_let_stmt()
        end
    elseif statement then
        return statement(is_let, is_async)
    else
        return parse_call_assign()
    end
end

function parse_if_stmt()
    Self:next()

    local cond = parse_expr()
    local body = parse_block()
    local else_body

    if consume(Keyword.Else) then
        if next_is(Keyword.If) then
            else_body = parse_if_stmt()
        else
            else_body = parse_block()
        end
    end

    return ast.if_stmt(cond, body, else_body)
end

function parse_do_stmt()
    Self:next()

    local body = parse_block()

    return ast.do_stmt(body)
end

function parse_while_stmt()
    Self:next()

    local cond = parse_expr()
    local body =  parse_block()

    return ast.while_stmt(cond, body)
end

function parse_for_stmt()
    Self:next()

    local var = parse_ident()

    local token = Self.token
    if token == Op.Assign then
        return parse_fornum_stmt(var)
    elseif token == Token.Comma or Keyword.In then
        return parse_foreach_stmt(var)
    else
        Self:error("expected '=' or 'in' near " .. token)
    end
end

function parse_fornum_stmt(var)
    Self:next() -- skip '='

    local init = parse_expr()
    expect(Token.Comma)

    local last = parse_expr()

    local step
    if consume(Token.Comma) then
        step = parse_expr()
    end

    local body = parse_block()

    return ast.for_stmt(var, init, last, step, body)
end

function parse_foreach_stmt(var)
    local vars = {var}

    while consume(Token.Comma) do
        vars[#vars + 1] = parse_ident()
    end

    expect(Keyword.In)

    local exps = parse_expr_list(ast, ls)
    local body = parse_block()

    return ast.foreach_stmt(vars, exps, body)
end

function parse_let_stmt()
    local locals = {}

    repeat
        locals[#locals + 1] = parse_ident()
    until not consume(Token.Comma)

    local exps

    local token = Self.token
    if token ~= Token.Semicolon then
        if token ~= Op.Assign then
            expect(Token.Semicolon)
        end

        Self:next()
        exps = parse_expr_list()
    end

    expect(Token.Semicolon)

    return ast.local_decl(locals, exps)
end

function parse_return_stmt()
    Self:next()

    local exps

    if not next_is(Token.Semicolon) then
        exps = parse_expr_list()
    end

    expect(Token.Semicolon)

    return ast.return_stmt(exps), true
end

function parse_break_stmt()
    local line = Self.linenumber
    Self:next()

    expect(Token.Semicolon)

    return ast.break_stmt(line), true
end

function parse_continue_stmt()
    local line = Self.linenumber
    Self:next()

    expect(Token.Semicolon)

    return ast.continue_stmt(line), true
end

function parse_label_stmt()
    Self:next()

    local label = parse_ident()
    expect(Token.Label)

    return ast.label_stmt(label)
end

function parse_goto_stmt()
    Self:next()

    local label = parse_ident()
    expect(Token.Semicolon)

    return ast.goto_stmt(label)
end

function parse_func_stmt(is_local, is_async)
    Self:next()

    local v = parse_ident()

    local needself = false
    if not is_local then
        while next_is(Op.Dot) do
            v = parse_field_expr(v)
        end

        if next_is(Token.Colon) then
            needself = true
            v = parse_field_expr(v)
        end
    end

    local params, body = parse_body(needself)

    return ast.function_decl(is_local, v, body, params, is_async)
end

function parse_class_stmt(is_local)
    Self:next()

    local c_ident = parse_ident()

    if not is_local then
        while next_is(Op.Dot) do
            c_ident = parse_field_expr(c_ident)
        end
    end

    -- local parent
    -- if consume(Keyword.Extends) then
    --     parent = parse_ident()
    -- end

    expect(Token.LBrace)

    local ctor = nil
    local c_body = {}

    while not next_is(Token.RBrace) do
        local token = peek()

        local is_static = false
        local is_async  = false

        if token == Keyword.Static then
            is_static = true
            token = Self:next()
        end

        if token == Keyword.Async then
            is_async = true
            Self:next()
        end

        local ident = parse_ident()
        token = peek()

        if token == Op.Assign then
            if is_async then Self:error(async_error_msg, Self.lastline) end
            Self:next()

            ident.field = true
            ident.is_static = is_static
            ident.body = parse_expr()

            table.insert(c_body, ident)

            expect(Token.Semicolon)
        elseif token == Token.LParens then
            local params, body = parse_body(true)
            local method = ast.function_decl(false, ident, body, params, is_async, is_static)

            if ident.value == "new" and not is_static then
                if ctor then
                    Self:error("duplicate constructor definition for class " .. ident)
                end

                ctor = true
                method.ctor = true
                table.insert(c_body, method)
            else
                table.insert(c_body, method)
            end
        else
            Self:error(
                format("unexpected %s in class", Self.token)
            )
        end
    end

    expect(Token.RBrace)

    return ast.class_decl(c_ident, c_body, is_local)
end

function parse_use_stmt()
    Self:next()

    local uses = {}

    repeat
        local ident = parse_ident()

        local locals
        while next_is(Op.Dot) do
            Self:next()

            if consume(Token.LBrace) then
                locals = {}

                while not next_is(Token.RBrace) do
                    table.insert(locals, parse_ident());

                    if not consume(Token.Comma) then
                        break
                    end
                end

                expect(Token.RBrace)

                break
            else
                ident = ast.expr_property(ident, parse_ident())
            end
        end

        if locals and #locals == 0 then
            continue
        end

        if not locals and ident.kind == "MemberExpression" then
            ident, locals = ident.object, {ident.property}
        end

        table.insert(uses, {
            ident  = ident,
            locals = locals
        })
    until not (consume(Token.Comma))

    expect(Token.Semicolon)

    return ast.use_stmt(uses)
end

function parse_args()
    local line = Self.lastline

    expect(Token.LParens)

    if line ~= Self.lastline then
        Self:error("ambiguous syntax (function call x new statement)")
    end

    local args
    if not next_is(Token.RParens) then
        args = parse_expr_list()
    end

    expect(Token.RParens)

    return args
end

function parse_body(needself)
    local old_varargs = Self.varargs
    Self.varargs = false

    local params = parse_params(needself)
    local body = parse_block()

    Self.varargs = old_varargs

    return params, body
end

function parse_block()
    expect(Token.LBrace)
    local body = {}

    local token = Self.token
    local stmt, is_last = nil, false
    while not is_last and token ~= Token.EOF and token ~= Token.RBrace do
        stmt, is_last = parse_stmt()
        body[#body + 1] = stmt
        token = Self.token
    end

    expect(Token.RBrace)

    return body
end

local assignments_ops = {
    [Op.Assign] = "=",
    [Op.AddAssign] = "+",
    [Op.SubAssign] = "-",
    [Op.DivAssign] = "/",
    [Op.MulAssign] = "*",
    [Op.ModAssign] = "%",
    [Op.PowAssign] = "^",
    [Op.ConAssign] = ".."
}

function parse_assignment(vlist, var, vk)
    vlist[#vlist + 1] = var

    if consume(Token.Comma) then
        local n_var, n_vk = parse_primary_expr()

        return parse_assignment(vlist, n_var, n_vk)
    else
        local token = peek()
        local op = assignments_ops[token]

        if not op then
            Self:error("expected assignment", Self.lastline)
        end

        Self:next()

        local exps = parse_expr_list()
        expect(Token.Semicolon)

        return ast.assignment_expr(op, vlist, exps)
    end
end

local call_expressions = {
    CallExpression  = 1,
    SendExpression  = 1,
}

function parse_call_assign()
    local var, token = parse_expr()

    if (var == Token.Ident or var.kind == "MemberExpression") and (assignments_ops[token] or token == Token.Comma) then
        return parse_assignment({}, var, token)
    end

    if call_expressions[var.kind] then
        expect(Token.Semicolon)

        return ast.statement_expr(var)
    elseif token ~= Token.Semicolon then
        return ast.return_stmt({var}), true
    else
        Self:error("expected statement")
    end
end

function parse_params(needself)
    expect(Token.LParens)

    local args = {}

    if needself then
        args[1] = ast.identifier(
            Token.Ident("self")
        )
    end

    if not next_is(Token.RParens) then
        repeat
            if next_is(Token.Ident) then
                local ident = parse_ident()
                args[#args + 1] = ident

                if consume(Token.Colon) then
                    ident.type = parse_ident("'type' for argument")
                end

                if consume(Op.Assign) then
                    ident.default_value = parse_expr()
                end
            elseif consume(Op.Ellipsis) then
                args[#args + 1] = ast.expr_vararg()
                Self.varargs = true
                break
            else
                Self:error("expected 'argument<name>'")
            end
        until not consume(Token.Comma)
    end

    expect(Token.RParens)

    return args
end

function parse_ident(expected)
    local token = Self.token
    if token == Token.Ident then
        Self:next()

        return ast.identifier(token)
    else
        Self:error(format("unexpected %s, expected %s", token, expected or "identifier"))
    end
end

return parse