local operator = include("lau/parser/operator.lua")
local ast 	   = include("lau/ast.lua")

local format = string.format

local lexer = Lau.lexer
local Keyword = lexer.Keyword
local Literal = lexer.Literal
local Op = lexer.Op
local Token = lexer.Token


local function value_to_key(tbl)
	for k, v in ipairs(tbl) do
		tbl[v], tbl[k] = true, nil
	end
end

local function has_value(t, v)
	for i = 1, #t do
		if t[i] == v then
			return true
		end
	end

	return false
end

local function is_literal(v)
	return Literal[v.key] and true or false
end

local consume, expect, next_is, next_is_in, peek

local parse, parse_args, parse_body, parse_block, parse_assignment,
	parse_call_assign, parse_params, parse_ident

local parse_expr, parse_expr_list, parse_binop_expr, parse_unop_expr, parse_primary_expr,
	parse_simple_expr, parse_field_expr, parse_bracket_expr,
	parse_table_expr, parse_arrow_func_expr

local parse_stmt, parse_if_stmt, parse_do_stmt, parse_while_stmt, parse_for_stmt,
	parse_foreach_stmt, parse_let_stmt, parse_return_stmt, parse_break_stmt, parse_continue_stmt,
	parse_label_stmt, parse_goto_stmt, parse_func_stmt, parse_class_stmt

local self
function parse(s)
	self = s
	self.varargs = true
	self.has_await = false
	self.old_has_await = {}

	local tree = {}
	local stmt, is_last = nil, false

	while not is_last and self.token ~= Token.EOF do
		stmt, is_last = parse_stmt()
		tree[#tree + 1] = stmt
    end

    if self.token ~= Token.EOF then
    	expect(Token.EOF)
	end

    return ast.chunk(tree, self.chunkname)
end

function consume(value)
	local token = self.token

	if token == value then
		self:next()
		return token
	end

	return false
end

function expect(expected)
	if self.token == expected then
		self:next()
		return true
	else
		self:error(format("expected %s", expected), self.lastline)
	end
end

function next_is(expected)
	local token = self.token

	return token == expected
end

function next_is_in(possibilities)
	local token = self.token

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
	local token = self.token

	-- print("\n----------------")
	-- PrintType(debug.getinfo(2))

	if token == Token.EOF then
		self:error("unexpected EOF")
	else
		return token
	end
end

local function self_save(name, new)
	local old = self[name]
	self[name] = new

	return old
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
    local op = self.token

    while is_binop(op) and left_priority(op) > limit do
        self:next()

        local v2, nextop = parse_binop_expr(right_priority(op))
        v = ast.expr_binop(op, v, v2)
        op = nextop
    end

    return v, op
end

function parse_unop_expr()
    local tk = self.token

    if tk == Op.Not or tk == Op.Sub or tk == Op.Len then
        self:next()
        local v = parse_binop_expr(operator.unary_priority)

        return ast.expr_unop(tk, v)
    else
        return parse_simple_expr()
    end
end

function parse_primary_expr()
    local v, vk

    local line

    if next_is(Token.LParens) then
    	local arrow_func = parse_arrow_func_expr()
    	if arrow_func then
    		return arrow_func
    	end

    	self:next()

    	local expr = parse_expr()
        expect(Token.RParens)
        vk, v = "expr", ast.expr_parentheses(expr)
    elseif next_is(Token.Ident) then
        vk, v = "var", parse_ident()

        if next_is(Token.Arrow) then
        	return parse_arrow_func_expr(v)
        end
    elseif consume(Keyword.New) then
    	local c = parse_ident()
    	local args = parse_args()

        vk, v = "call", ast.new_expr(c, args)
    elseif consume(Keyword.Await) then
    	self.await = true
        vk, v = "call", ast.await_expr(parse_expr())
    else
    	self:error("unexpected symbol " .. peek())
    end

	::REPEAT::

	local token = self.token

	if token == Op.Dot then
		self:next()

		if not next_is_in{Literal, Token.Ident} or next_is(Literal.Nil) then
			self:error(
				format("unexpected %s, expected <key>", peek())
			)
		end

		local key = peek()
		self:next()

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
		self:next()

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
    local token = self.token
    if not token then
    	self:error("expected expression", self.lastline)
    end

    if is_literal(token) then
    	self:next()

    	return ast.literal(token)
	elseif token == Op.Ellipsis then
		if self:next() == Token.Arrow then
			return parse_arrow_func_expr(ast.expr_vararg(), true)
		end

        if not self.varargs then
        	self:error("cannot use '...' outside a vararg function", self.lastline)
        end

        return ast.expr_vararg()
    elseif token == Token.LBrace then
        return parse_table_expr()
    elseif token == Keyword.Fn then
        self:next()

        local params, body = parse_body()

        return ast.expr_function(body, params, body.is_async)
    else
        return parse_primary_expr()
    end

    self:next()

    return e
end

function parse_field_expr(v)
	self:next()

	local key = parse_ident()

	return ast.expr_property(v, key)
end

function parse_bracket_expr()
    self:next()

    local v = parse_expr()
    expect(Token.RBracket)

    return v
end

function parse_table_expr()
    expect(Token.LBrace)

    local kvs = {}

    while not next_is(Token.RBrace) do
        local key, val

		local token = self.token
        if token == Token.LBracket then
        	if self:lookahead() == Literal.Nil then
        		self:error("unexpected 'nil' key")
        	end

        	key = parse_bracket_expr()
        	expect(Token.Colon)
        elseif is_literal(token) or token == Token.Ident then
        	if token == Literal.Nil then
        		self:error("unexpected 'nil' key")
        	end


			if self:lookahead() == Token.Colon then
				self:next()
				self:next()

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

function parse_arrow_func_expr(name, vararg)
	local reset = self:fake_llex()
	local params

	local o_varargs = self_save("varargs", false)
	local o_await = self_save("await", false)

	if vararg then
		self.varargs = true
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
		local token, body = self:next()

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

		self_save("varargs", o_varargs)
		local is_async = self_save("await", o_await)

		return ast.expr_function(body, params, is_async)
	end
end

--[[
	Statements
]]

local statements
local let_statements
function parse_stmt()
	local is_let = consume(Keyword.Let)

	if not statements then
		statements = {
			[Keyword.If] = parse_if_stmt,
			[Keyword.Do] = parse_do_stmt,
			[Keyword.While] = parse_while_stmt,
			[Keyword.For] = parse_for_stmt,
			[Keyword.ForEach] = parse_foreach_stmt,
			[Keyword.Return] = parse_return_stmt,
			[Keyword.Break] = parse_break_stmt,
			[Keyword.Continue] = parse_continue_stmt,
			[Token.Label] = parse_label_stmt,
			[Keyword.Goto] = parse_goto_stmt,
			[Keyword.Fn] = parse_func_stmt,
			[Keyword.Class] = parse_class_stmt,
			[Keyword.Enum] = function()
				self:error("Enum is not implemented yet")
			end
		}

		let_statements = {Keyword.Fn, Keyword.Class, Keyword.Enum}
	end

	local statement = statements[self.token]

	if is_let then
		if next_is_in(let_statements) then
			return statement(true)
		else
			return parse_let_stmt()
		end
	elseif statement then
		return statement(is_let)
	else
		return parse_call_assign()
	end
end

function parse_if_stmt()
	self:next()

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
	self:next()

	local body = parse_block()

	if consume(Keyword.While) then
		local cond = parse_expr()
    	expect(Token.Semicolon)

		return ast.do_while_stmt(cond, body)
	end

	return ast.do_stmt(body)
end

function parse_while_stmt()
	self:next()

	local cond = parse_expr()
	local body =  parse_block()

	return ast.while_stmt(cond, body)
end

function parse_for_stmt()
	self:next()

	expect(Token.LParens)

	local var = parse_ident()
	expect(Op.Assign)

	local init = parse_expr()
	expect(Token.Comma)

	local last = parse_expr()
	local step

	if consume(Token.Comma) then
		step = parse_expr()
	end

	expect(Token.RParens)

	local body = parse_block()

	return ast.for_stmt(var, init, last, step, body)
end

function parse_foreach_stmt()
	self:next()

	expect(Token.LParens)

	local vars = {}

	repeat
		vars[#vars + 1] = parse_ident()
	until not consume(Token.Comma)

	expect(Keyword.In)

	local exps = parse_expr_list(ast, ls)

	expect(Token.RParens)

	local body = parse_block()

	return ast.foreach_stmt(vars, exps, body)
end

function parse_let_stmt()
	local locals = {}

	repeat
		locals[#locals + 1] = parse_ident()
	until not consume(Token.Comma)

	local exps

	local token = self.token
	if token ~= Token.Semicolon then
		if token ~= Op.Assign then
			expect(Token.Semicolon)
		end

		self:next()
		exps = parse_expr_list()
	end

	expect(Token.Semicolon)

	return ast.local_decl(locals, exps)
end

function parse_return_stmt()
	self:next()

	local exps

	if not next_is(Token.Semicolon) then
		exps = parse_expr_list()
	end

	expect(Token.Semicolon)

	return ast.return_stmt(exps), true
end

function parse_break_stmt()
	local line = self.linenumber
	self:next()

	expect(Token.Semicolon)

	return ast.break_stmt(line), true
end

function parse_continue_stmt()
	local line = self.linenumber
	self:next()

	expect(Token.Semicolon)

	return ast.continue_stmt(line), true
end

function parse_label_stmt()
	self:next()

	local label = parse_ident()
	expect(Token.Label)
	expect(Token.Semicolon)

	return ast.label_stmt(label)
end

function parse_goto_stmt()
	self:next()

	local label = parse_ident()
	expect(Token.Semicolon)

	return ast.goto_stmt(label)
end

function parse_func_stmt(is_local)
	self:next()

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

	return ast.function_decl(is_local, v, body, params, body.is_async)
end

function parse_class_stmt(is_local)
	self:next()

	local c_ident = parse_ident()

	local parent
	if consume(Keyword.Extends) then
		parent = parse_ident()
	end

	expect(Token.LBrace)

	local ctor = nil
	local c_body = {}

	while not next_is(Token.RBrace) do
		local is_static = false

		if peek() == Keyword.Static then
			is_static = true
			self:next()
		end

		local ident = parse_ident()

		if next_is(Op.Assign) then
			self:next()

			ident.field = true
			ident.is_static = is_static
			ident.body = parse_expr()

			table.insert(c_body, ident)

			expect(Token.Semicolon)
		elseif next_is(Token.LParens) then
			local params, body = parse_body(true)
			local method = ast.function_decl(false, ident, body, params, body.is_async, is_static)

			if c_ident.value == ident.value then
				if ctor then
					self:error("duplicate constructor definition for class " .. ident)
				end

				if is_static then
					self:error(format("constructor for %s is marked 'static'", ident))
				end

				local last = body[#body]
				if last and last.kind == "ReturnStatement" then
					self:error("can't have a return statement in class constructor", ident.line)
				end

				ctor = true
				method.ctor = true
				table.insert(c_body, method)
			else
				table.insert(c_body, method)
			end
		else
			self:error(
				format("unexpected %s in class", self.token)
			)
		end
	end

	expect(Token.RBrace)

	return ast.class_decl(c_ident, c_body, is_local)
end

function parse_args()
	local line = self.lastline

	expect(Token.LParens)

	if line ~= self.lastline then
		self:error("ambiguous syntax (function call x new statement)")
	end

	local args
	if not next_is(Token.RParens) then
		args = parse_expr_list()
	end

	expect(Token.RParens)

	return args
end

function parse_body(needself)
	local o_varargs = self_save("varargs", false)
	local o_await = self_save("await", false)

	local params = parse_params(needself)

	local body = parse_block()
	body.is_async = self.await

	self_save("varargs", o_varargs)
	self_save("await", o_await)

	return params, body
end

function parse_block()
	expect(Token.LBrace)
	local body = {}

	local token = self.token
	local stmt, is_last = nil, false
	while not is_last and token ~= Token.EOF and token ~= Token.RBrace do
		stmt, is_last = parse_stmt()
		body[#body + 1] = stmt
		token = self.token
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
    		self:error("expected assignment", self.lastline)
    	end

    	self:next()

        local exps = parse_expr_list()
    	expect(Token.Semicolon)

        return ast.assignment_expr(op, vlist, exps)
    end
end

local call_expressions = {
	CallExpression  = 1,
	SendExpression  = 1,
	AwaitExpression = 1,
	NewExpression   = 1
}

function parse_call_assign()
	local var, token = parse_expr()

	if (var == Token.Ident or var.kind == "MemberExpression") and (assignments_ops[token] or token == Token.Comma) then
		return parse_assignment({}, var, token)
	end

	if token ~= Token.Semicolon then
		return ast.return_stmt({
			var
		}), true
	elseif call_expressions[var.kind] then
		self:next()

        return ast.statement_expr(var)
	else
        self:error("expected statement")
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
				self.varargs = true
				break
			else
				self:error("expected 'argument<name>'")
			end
		until not consume(Token.Comma)
	end

	expect(Token.RParens)

	return args
end

function parse_ident(expected)
	local token = self.token
	if token == Token.Ident then
		self:next()

		return ast.identifier(token)
	else
		self:error(format("unexpected %s, expected %s", token, expected or "identifier"))
	end
end

return parse