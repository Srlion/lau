local lexer = lau.lexer
local Keyword = lexer.Keyword
local Literal = lexer.Literal
local Op = lexer.Op
local Token = lexer.Token

local function build(kind, node)
    node.kind = kind
    return node
end

local function does_multi_return(expr)
    local k = expr.kind
    return k == "CallExpression" or k == "SendExpression" or k == "Vararg"
end

local AST = {}

function AST.expr_function(body, params, default, is_async)
	return build("FunctionExpression", {
		body     = body,
		params   = params,
		default  = default,
		is_async = is_async
	})
end

function AST.function_decl(locald, id, body, params, default, is_async)
	return build("FunctionDeclaration", {
		locald   = locald,
        id		 = id,
        body	 = body,
        params	 = params,
		default  = default,
        is_async = is_async
    })
end

function AST.expr_unop(op, v)
    return build("UnaryExpression", {
    	operator = op,
    	argument = v
    })
end

function AST.expr_binop(op, expa, expb)
    local binop_body = (op != Op.Concat && {
    	operator = op,
    	left = expa,
    	right = expb
    })

    if binop_body then
        if op == Op.LAnd || op == Op.LOr then
            return build("LogicalExpression", binop_body)
        else
            return build("BinaryExpression", binop_body)
        end
    else
        return build("ConcatenateExpression", {
        	left = expa,
        	right = expb
        })
    end
end

function AST.literal(literal)
    return build("Literal", {value = literal.value, line = literal.line})
end

function AST.expr_vararg()
    return build("Vararg", {})
end

function AST.expr_table(keyvals)
    return build("Table", {keyvals = keyvals})
end

function AST.identifier(ident)
    return build("Identifier", {name = ident.value, line = ident.line})
end

function AST.func_parameters_decl(ast, params)
    if vararg then
		table.insert(params, AST.expr_vararg())
    end
    return params
end

function AST.if_stmt(cond, body, else_body)
    return build("IfStatement", {cond = cond,
    	body = body,
    	else_body = else_body
    })
end

function AST.while_stmt(cond, body)
    return build("WhileStatement", {cond = cond,
    	body = body
    })
end

function AST.for_stmt(var, init, last, step, body)
    return build("ForStatement", {
    	var = var,
    	init = init,
    	last = last,
    	step = step,
    	body = body
    })
end

function AST.local_decl(locals, exps)
    return build("LocalDeclaration", {locals = locals, expressions = exps})
end

function AST.set_expr_last(expr)
    if expr.bracketed and does_multi_return(expr) then
        expr.bracketed = nil
        return build("ExpressionValue", {value = expr})
    else
        return expr
    end
end

function AST.return_stmt(exps)
    return build("ReturnStatement", {arguments = exps})
end

function AST.break_stmt()
    return build("BreakStatement", {})
end

function AST.continue_stmt()
    return build("continue_stmt", {})
end

function AST.expr_property(obj, prop)
    return build("MemberExpression", {
        object = obj,
        property = prop,
        computed = false
    })
end

function AST.expr_index(obj, prop)
	local node = AST.expr_property(obj, prop)
	node.computed = true

	return node
end

function AST.expr_brackets(expr)
	return build("BracketedExpression", {
		value = expr
	})
end

function AST.label_stmt(label)
    return build("LabelStatement", {label = label})
end

function AST.goto_stmt(label)
    return build("GotoStatement", {label = label})
end

function AST.expr_method_call(v, method, args)
    return build("SendExpression", {
    	receiver = v,
    	method = method,
    	arguments = args
	})
end

function AST.statement_expr(expr)
    return build("ExpressionStatement", {expression = expr})
end

function AST.expr_function_call(v, args)
    return build("CallExpression", {
    	callee = v,
    	arguments = args
    })
end

function AST.assignment_expr(op, vars, exps)
    return build("AssignmentExpression", {
        operator = op,
        left = vars,
        right = exps
    })
end

function AST.await_expr(expr)
    return build("AwaitExpression", {expression = expr})
end

function AST.chunk(body, chunkname)
    return build("Chunk", {
        body = body,
        chunkname = chunkname
    })
end

return AST