local strbyte, strsub = string.byte, string.sub

local lexer   = lau.lexer
local Keyword = lexer.Keyword
local Literal = lexer.Literal
local Op 	  = lexer.Op
local Token   = lexer.Token

local function replace_cc(c)
    local esc = {
        ['\a'] = [[\a]], ['\b'] = [[\b]], ['\f'] = [[\f]], ['\n'] = [[\n]], ['\r'] = [[\r]], ['\t'] = [[\t]], ['\v'] = [[\v]]
    }
    return esc[c] and esc[c] or ('\\' .. string.format("%d", string.byte(c)))
end

local function escape(s)
    s = string.gsub(s, "[\"\\]", "\\%1")
    return string.gsub(s, "%c", replace_cc)
end

local concat = table.concat
local format = string.format

local function is_const(node, val)
    return node.kind == "Literal" and node.value == val
end

local function comma_sep_list(ls, f)
	local n = #ls
    for i = 1, n do
    	f(ls[i], i == n)
    end
end

local function add_default_values(self, defaults)
    if (!defaults) then return end

    for k, v in ipairs(defaults) do
        self:add_line("if(")
        self:expr_emit(v.ident)
        self:add_line("==nil)then ")
        self:expr_emit(v.ident)
        self:add_line("=")
        self:expr_emit(v.value)
        self:add_line(";end;")
    end
end

local function table_to_true(tbl)
    for k, v in ipairs(tbl) do
        tbl[v], tbl[k] = true, nil
    end
    return tbl
end

local ReplacedKeyword = table_to_true({
    "end", "then", "elseif", "repeat", "until", "not", "or"
})

local CHANGE_KEYWORD = true
local function should_replace(v)
    if (ReplacedKeyword[v.value]) then
        CHANGE_KEYWORD = false
        return true
    end
end

local ASYNC_FUNCTION = "LAU_ASYNC"
local AWAIT_FUNCTION = "LAU_AWAIT"
local NEW_FUNCTION   = "LAU_NEW"

local FUNCTION_NAMES = {}
local function GET_FUNCTION(name)
    return FUNCTION_NAMES[name]
end

local function SET_FUNCTION(name, value)
    FUNCTION_NAMES[name] = value
end

local StatementRule = {}
local ExpressionRule = {}

function StatementRule:Chunk(node)
    self:list_emit(node.body)
end

function StatementRule:Text(node)
    self:add_line(node.text, node.line)
end

function StatementRule:FunctionDeclaration(node)
	if (node.locald) then
		self:add_line("local ")
    	self:expr_emit(node.id)
		self:add_line(";")
	end

    self:expr_emit(node.id)
	self:add_line("=")

    if (node.is_async) then
        self:add_line(ASYNC_FUNCTION .. "(")
    end

	self:add_line("function(")

	comma_sep_list(node.params, function(_node, last)
        self:expr_emit(_node)
        if (!last) then
        	self:add_line(",")
        end
    end)

	self:add_line(")")

    add_default_values(self, node.default)

    self:add_section(node.body, node.is_async)

    if (node.is_async) then
        self:add_line(")")
    end
end

local function add_fields(self, fields, is_static)
    for i = 1, #fields do
        local field = fields[i]
        if (is_static) then
            if (!field.is_static) then continue end
        elseif (field.is_static) then
            continue
        end

        if (should_replace(field)) then
            field.value = "[\"" .. field.value .. "\"]"
            CHANGE_KEYWORD = true
        end

        self:expr_emit(field)
        self:add_line("=")
        self:expr_emit(field.body)
        self:add_line(",")
    end
end

function add_methods(self, methods, is_static)
    for i = 1, #methods do
        local method = methods[i]
        if (is_static) then
            if (!method.is_static) then continue end
        elseif (method.is_static) then
            continue
        end

        local id = method.id
        if (should_replace(id)) then
            id.value = "[\"" .. id.value .. "\"]"
            CHANGE_KEYWORD = true
        end

        self:emit(method)
    end
end

function StatementRule:ClassDeclaration(node)
    if (node.is_local) then
        self:add_line("local ")
        self:expr_emit(node.name)
        self:add_line(";")
    end

    self:add_line("do ")

    local class_iden = node.name

    self:expr_emit(node.name)
    self:add_line("={class={};};local index_table={__index=")

    self:expr_emit(node.name)
    self:add_line(".class};")

    local body = node.body
    for i = 1, #body do
        local v = body[i]

        local is_static = v.is_static

        local id = v.field && v || v.id
        local name = id.value

        if (name == "tostring" && !v.ctor && !v.field) then
            id.value = "__tostring"

            if (!is_static) then
                self:add_line("index_table.")
                self:emit(v)
            else
                self:add_line("setmetatable(")
                self:expr_emit(class_iden)
                self:add_line(", {")
                self:emit(v)
                self:add_line("});")
            end

            continue
        end

        self:expr_emit(class_iden)

        if (!v.ctor && !is_static) then
            self:add_line(".class")
        end

        if (should_replace(id)) then
            id.value = "[\"" .. name .. "\"]"
            CHANGE_KEYWORD = true
        else
            id.value = "." .. name
        end

        if (v.ctor) then
            table.remove(v.params, 1) -- remove self

            table.insert(v.body, 1, {
                kind = "Text",
                text = "local self=setmetatable({}, index_table);"
            })

            table.insert(v.body, {
                kind = "Text",
                text = "return self;"
            })

            self:emit(v)
        elseif (v.field) then
            self:expr_emit(v)
            self:add_line("=")
            self:expr_emit(v.body)
            self:add_line(";")
        else
            self:emit(v)
        end
    end

    self:add_line("end;")
end

function StatementRule:IfStatement(node)
    self:add_line("if ")
    self:expr_emit(node.cond)
    self:add_line(" then ")
    self:list_emit(node.body)

    local else_body = node.else_body
    while (else_body) do
        local cond = else_body.cond

        if (cond) then
            self:add_line("elseif ")
            self:expr_emit(cond)
            self:add_line(" then ")
            self:list_emit(else_body.body)
        else
            self:add_line("else ")
            self:list_emit(else_body)
        end

        else_body = else_body.else_body
    end

    self:add_line("end;")
end

function StatementRule:WhileStatement(node)
    self:add_line("while ")
    self:expr_emit(node.cond)
    self:add_line(" do ")
    self:add_section(node.body)
end

function StatementRule:DoWhileStatement(node)
    self:add_line("repeat ")
    self:list_emit(node.body)
    self:add_line("until!(")
    self:expr_emit(node.cond)
    self:add_line(")")
end

function StatementRule:ForStatement(node)
    self:add_line("for ")
    self:expr_emit(node.var)
    self:add_line("=")
    self:expr_emit(node.init)
    self:add_line(",")
    self:expr_emit(node.last)

    local step = node.step
    if step and not is_const(step, 1) then
        self:add_line(",")
        self:expr_emit(step)
    end

    self:add_line(" do ")
    self:add_section(node.body)
end

function StatementRule:LocalDeclaration(node)
    self:add_line("local ")
    self:expr_list(node.locals)

    local expressions = node.expressions
    if (expressions) then
        self:add_line("=")
        self:expr_list(expressions)
    end

    self:add_line(";")
end

function StatementRule:ReturnStatement(node)
    self:add_line("return")

    local args = node.arguments
    if (args) then
        self:add_line(" ")
        self:expr_list(args)
    end

    self:add_line(";")
end

function StatementRule:BreakStatement(node)
    self:add_line("break;", node.line)
end

function StatementRule:ContinueStatement(node)
    self:add_line("continue;", node.line)
end

function StatementRule:LabelStatement(node)
    self:add_line("::")
    self:expr_emit(node.label)
    self:add_line("::;")
end

function StatementRule:GotoStatement(node)
   self:add_line("goto ")
   self:expr_emit(node.label)
   self:add_line(";")
end

function StatementRule:ExpressionStatement(node)
    self:expr_emit(node.expression)
    self:add_line(";")
end

function StatementRule:AssignmentExpression(node)
    if node.operator == '=' then
        self:expr_list(node.left)
        self:add_line("=", node.line)
        self:expr_list(node.right)
    else
        local left = node.left[1]

        self:expr_emit(left)
        self:add_line("=")
        self:expr_emit(left)
        self:add_line(node.operator)
        self:add_line("(")
        self:expr_emit(node.right[1])
        self:add_line(")")
    end
    self:add_line(";")
end

function ExpressionRule:BracketedExpression(node)
    self:add_line("(")
    self:expr_emit(node.value)
    self:add_line(")")
end

function ExpressionRule:Identifier(node)
    local v = node.value
    if (CHANGE_KEYWORD && ReplacedKeyword[v]) then
        v = "​" .. v
    end
    self:add_line(v, node.line)
end

function ExpressionRule:Literal(node)
    local val = node.value
    local key = node.key
    if (key == "String") then
        val = format("\"%s\"", escape(val))
    elseif (key == "Nil") then
        val = "nil"
    end
    self:add_line(val, node.line)
end

function ExpressionRule:FunctionExpression(node)
    if (node.is_async) then
        self:add_line(ASYNC_FUNCTION .. "(")
    end

    self:add_line("function(")

    comma_sep_list(node.params, function(_node, last)
        self:expr_emit(_node)
        if (!last) then
            self:add_line(",")
        end
    end)

    self:add_line(")")

    add_default_values(self, node.default)

    self:add_section(node.body, true)

    if (node.is_async) then
        self:add_line(")")
    end
end

function ExpressionRule:MemberExpression(node)
    self:expr_emit(node.object)

    local prop = node.property
    if node.computed then
        self:add_line("[")
        self:expr_emit(prop)
        self:add_line("]")
    elseif (should_replace(prop)) then
        self:add_line("[\"", prop.line)
        self:expr_emit(prop)
        self:add_line("\"]")
        CHANGE_KEYWORD = true
    else
        self:add_line(".")
        self:expr_emit(prop)
    end
end

function ExpressionRule:Vararg(node)
    self:add_line("...", node.line)
end

function ExpressionRule:BinaryExpression(node)
    local oper = tostring(node.operator)
    oper = strsub(oper, 2, #oper - 1)

    self:expr_emit(node.left)
    self:add_line(oper)
    self:expr_emit(node.right)
end
ExpressionRule.LogicalExpression = ExpressionRule.BinaryExpression

function ExpressionRule:UnaryExpression(node)
    local oper = tostring(node.operator)
    oper = strsub(oper, 2, #oper - 1)

    self:add_line(oper)
    self:expr_emit(node.argument)
end

function ExpressionRule:ConcatenateExpression(node)
    self:expr_emit(node.left)
    self:add_line(" ..")
    self:expr_emit(node.right)
end

function ExpressionRule:TableExpression(node)
    local last = #node.keyvals
    self:add_line("{")

    for i = 1, last do
        local kv = node.keyvals[i]
        local key, val = kv[1], kv[2]
        if (key) then
            if (should_replace(key)) then
                self:add_line("[\"", key.line)
                self:expr_emit(key)
                self:add_line("\"]=")
                CHANGE_KEYWORD = true
            elseif (key.key == "Ident") then
                self:expr_emit(key)
                self:add_line("=")
            else
                self:add_line("[")
                self:expr_emit(key)
                self:add_line("]=")
            end
            self:expr_emit(val)
        else
            self:expr_emit(val)
        end

        if (i != last) then
            self:add_line(",")
        end
    end

    self:add_line("}")
end

function ExpressionRule:CallExpression(node)
    self:expr_emit(node.callee)
    self:add_line("(")

    local args = node.arguments
    if (args) then
        self:expr_list(args)
    end

    self:add_line(")")
end

function ExpressionRule:SendExpression(node)
    self:expr_emit(node.receiver)
    self:add_line(":")
    self:expr_emit(node.method)
    self:add_line("(")

    local args = node.arguments
    if (args) then
        self:expr_list(args)
    end

    self:add_line(")")
end

function StatementRule:DoStatement(node)
    self:add_line("do ")
    self:add_section(node.body)
end

function ExpressionRule:NewExpression(node)
    local name = node.name
    local line = name.line

    self:add_line(NEW_FUNCTION .. "(", line)
    self:expr_emit(name)
    self:add_line(",\"", line)
    self:expr_emit(name)
    self:add_line("\"", line)

    local args = node.arguments
    if (args) then
        self:add_line(",", line)
        self:expr_list(args)
    end

    self:add_line(")")
end

function ExpressionRule:AwaitExpression(node)
    self:add_line(AWAIT_FUNCTION .. "(")
    self:expr_emit(node.expression)
    self:add_line(")")
end

local generated_names = {}

local first_load = true
local rep = string.rep
local function generate(tree, name, no_lines)
	local self = {}

    local code = {" "}

    if !name || (name && !generated_names[name]) then
        if (name) then
            generated_names[name] = true
        end
    end

    if (no_lines) then
        function self:add_line(str)
            code[#code + 1] = str
        end
    else
        local current = 1
        function self:add_line(str, line)
            if (line && line > current) then
                code[#code + 1] = rep("\n", line - current)
                current = line
            end

            code[#code + 1] = str
        end
    end

    function self:add_section(body, omit_end)
        self:list_emit(body)
        self:add_line("end")
        if not omit_end then
            self:add_line(";")
        end
    end

    function self:expr_emit(node)
        local rule = ExpressionRule[node.kind]
        if not rule then error("cannot find an expression rule for " .. node.kind) end
        return rule(self, node)
    end

    function self:expr_list(exps)
        comma_sep_list(exps, function(node, last)
            self:expr_emit(node)
            if (!last) then
                self:add_line(",")
            end
        end)
    end

    function self:emit(node)
        local rule = StatementRule[node.kind]
        if not rule then error("cannot find a statement rule for " .. node.kind) end
        rule(self, node)
    end

    function self:list_emit(node_list)
        for i = 1, #node_list do
            self:emit(node_list[i])
        end
    end

    self:emit(tree)

    return concat(code)
end

return generate, function(name)
    return function(tree, debug)
        return generate(tree, name, false)
    end
end