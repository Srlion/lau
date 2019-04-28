local lexer   = Lau.lexer
local Keyword = lexer.Keyword
local Literal = lexer.Literal
local Op 	  = lexer.Op
local Token   = lexer.Token

local concat = table.concat
local gsub   = string.gsub
local byte   = string.byte
local strsub = string.sub
local format = string.format

local function replace_cc(c)
    local esc = {
        ['\a'] = [[\a]], ['\b'] = [[\b]], ['\f'] = [[\f]], ['\n'] = [[\n]], ['\r'] = [[\r]], ['\t'] = [[\t]], ['\v'] = [[\v]]
    }
    return esc[c] and esc[c] or ('\\' .. format("%d", byte(c)))
end

local function escape(s)
    s = gsub(s, "[\"\\]", "\\%1")
    return gsub(s, "%c", replace_cc)
end

local function is_const(node, val)
    return node.kind == "Literal" and node.value == val
end

local function comma_sep_list(ls, f)
	local n = #ls
    for i = 1, n do
    	f(ls[i], i == n)
    end
end

local is_types = {
    func   = "isfunction",
    bool   = "isbool",
    matrix = "ismatrix",
    number = "isnumber",
    string = "isstring",
    color  = "IsColor",
    entity = "isentity",
    table  = "istable",
    angle  = "isangle",
    vector = "isvector",
    panel  = "ispanel"
}

local types_to_replace = {
    player = "Player"
}

bad_argument_message = [[
error("bad argument #%d to '%s' (expected '%s' got '" .. type(%s) .. "')", 2)]]
local function check_params(self, params, func_name)
    if not params then return end

    for i = 1, #params do
        local param = params[i]

        local type = param.type
        local value = param.default_value

        if not type and not value then
            goto CONTINUE
        end

        self:add_line("if(")

        if type then
            type = type.value

            local is_type = is_types[type:lower()]
            if is_type then
                self:add_line("!" .. is_type .. "(")
                self:expr_emit(param)
                self:add_line(")")
                type = is_type:sub(3)
            else
                type = types_to_replace[type:lower()] or type
                self:add_line("type(")
                self:expr_emit(param)
                self:add_line(")!=\"" .. type .. "\"")
            end

            self:add_line(")then ")

            if value then
                goto VALUE
            end

            self:add_line(format(bad_argument_message, i, func_name, type, param.value))
        else
            self:expr_emit(param)
            self:add_line("==")
            self:add_line("nil")
            self:add_line(")then ")
        end

        ::VALUE::

        if value then
            self:expr_emit(param)
            self:add_line("=")
            self:expr_emit(value)
        end

        self:add_line(";end;")

        ::CONTINUE::
    end
end

local function table_to_true(tbl)
    for k, v in ipairs(tbl) do
        tbl[v], tbl[k] = true, nil
    end
    return tbl
end

local ReplacedKeyword = table_to_true({
    "end", "then", "elseif", "repeat", "until", "not", "or", "function"
})

local CHANGE_KEYWORD = true
local function should_replace(v)
    if ReplacedKeyword[v.value] then
        CHANGE_KEYWORD = false
        return true
    end
end

Lau.Modules = {
    colon_call  = {
        pos = 1,
        "__CALL__"
    },
    async = {
        pos = 2,
        "__ASYNC__"
    },
    await = {
        "__AWAIT__",
        "await_failed"
    },
    new = {
        pos = 3,
        "__NEW__"
    },
    promise = {
        pos = 4,
        "Promise"
    }
}

local function get_name(name)
    return Lau.Modules[name][1]
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
	if node.locald then
		self:add_line("local ")
    	self:expr_emit(node.id)
		self:add_line(";")
	end

    self:expr_emit(node.id)
	self:add_line("=")

    if node.is_async then
        self:add_line(get_name("async") .. "(")
    end

	self:add_line("function(")

	comma_sep_list(node.params, function(_node, last)
        self:expr_emit(_node)
        if not last then
        	self:add_line(",")
        end
    end)

	self:add_line(")")

    check_params(self, node.params, node.id.value)

    self:add_section(node.body, node.is_async)

    if node.is_async then
        self:add_line(")")
    end
end

function StatementRule:ClassDeclaration(node)
    if node.is_local then
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

        local id = v.field and v or v.id
        local name = id.value

        if name == "tostring" and not v.ctor and not v.field then
            id.value = "__tostring"

            if not is_static then
                self:add_line("index_table.")
                self:emit(v)
            else
                self:add_line("setmetatable(")
                self:expr_emit(class_iden)
                self:add_line(", {")
                self:emit(v)
                self:add_line("});")
            end

            goto CONTINUE
        end

        self:expr_emit(class_iden)

        if not v.ctor and not is_static then
            self:add_line(".class")
        end

        if should_replace(id) then
            id.new_value = "[\"" .. name .. "\"]"
            CHANGE_KEYWORD = true
        else
            id.new_value = "." .. name
        end

        if v.ctor then
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
        elseif v.field then
            self:expr_emit(v)
            self:add_line("=")
            self:expr_emit(v.body)
            self:add_line(";")
        else
            if is_static then
                table.remove(v.params, 1)
            end

            self:emit(v)
        end

        ::CONTINUE::
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

        if cond then
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

function StatementRule:ForEachStatement(node)
    self:add_line("for ")
    self:expr_list(node.vars)
    self:add_line(" in ")
    self:expr_list(node.exps)
    self:add_line(" do ")
    self:add_section(node.body)
end

function StatementRule:LocalDeclaration(node)
    self:add_line("local ")
    self:expr_list(node.locals)

    local expressions = node.expressions
    if expressions then
        self:add_line("=")
        self:expr_list(expressions)
    end

    self:add_line(";")
end

function StatementRule:ReturnStatement(node)
    self:add_line("return")

    local args = node.arguments
    if args then
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

function ExpressionRule:ParenthesesExpression(node)
    self:add_line("(")
    self:expr_emit(node.value)
    self:add_line(")")
end

function ExpressionRule:Identifier(node)
    local v = node.new_value or node.value
    if CHANGE_KEYWORD and ReplacedKeyword[v] then
        v = "​" .. v
    end
    self:add_line(v, node.line)
end

function ExpressionRule:Literal(node)
    local val = node.value
    local key = node.key
    if key == "String" then
        val = format("\"%s\"", escape(val))
    elseif key == "Nil" then
        val = "nil"
    end
    self:add_line(val, node.line)
end

function ExpressionRule:FunctionExpression(node)
    if node.is_async then
        self:add_line(get_name("async") .. "(")
    end

    self:add_line("function(")

    comma_sep_list(node.params, function(_node, last)
        self:expr_emit(_node)
        if not last then
            self:add_line(",")
        end
    end)

    self:add_line(")")

    check_params(self, node.params, "function")

    self:add_section(node.body, true)

    if node.is_async then
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
    elseif should_replace(prop) then
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
        if key then
            if should_replace(key) then
                self:add_line("[\"", key.line)
                self:expr_emit(key)
                self:add_line("\"]=")
                CHANGE_KEYWORD = true
            elseif key.key == "Ident" then
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

        if i ~= last then
            self:add_line(",")
        end
    end

    self:add_line("}")
end

function ExpressionRule:CallExpression(node)
    self:expr_emit(node.callee)
    self:add_line("(")

    local args = node.arguments
    if args then
        self:expr_list(args)
    end

    self:add_line(")")
end

function ExpressionRule:SendExpression(node)
    local method = node.method
    local args = node.arguments

    if should_replace(method) then
        self:add_line(get_name("colon_call") .. "(")
        self:expr_emit(node.receiver)
        self:add_line(",\"" .. method.value .. "\",", method.line)
        CHANGE_KEYWORD = true
    else
        self:expr_emit(node.receiver)
        self:add_line(":")
        self:expr_emit(method)
        self:add_line("(")
    end

    if args then
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

    self:add_line(get_name("new") .. "(", line)
    self:expr_emit(name)
    self:add_line(",\"", line)
    self:expr_emit(name)
    self:add_line("\"", line)

    local args = node.arguments
    if args then
        self:add_line(",", line)
        self:expr_list(args)
    end

    self:add_line(")")
end

function ExpressionRule:AwaitExpression(node)
    self:add_line(get_name("await") .. "(")
    self:expr_emit(node.expression)
    self:add_line(")")
end

local SELF = {}
local SELF_Class = {__index = SELF}

function SELF:add_section(body, omit_end)
    self:list_emit(body)
    self:add_line("end")
    if not omit_end then
        self:add_line(";")
    end
end

function SELF:expr_emit(node)
    local rule = ExpressionRule[node.kind]
    if not rule then error("cannot find an expression rule for " .. node.kind) end
    return rule(self, node)
end

function SELF:expr_list(exps)
    comma_sep_list(exps, function(node, last)
        self:expr_emit(node)
        if not last then
            self:add_line(",")
        end
    end)
end

function SELF:emit(node)
    local rule = StatementRule[node.kind]
    if not rule then error("cannot find a statement rule for " .. node.kind) end
    rule(self, node)
end

function SELF:list_emit(node_list)
    for i = 1, #node_list do
        self:emit(node_list[i])
    end
end

local rep = string.rep
function SELF:ADDLINE_1(str, line)
    local code = self.code
    local current = self.current

    if line and line > current then
        code[#code + 1] = rep("\n", line - current)
        self.current = line
    end

    code[#code + 1] = str
end

function SELF:ADDLINE_2(str)
    local code = self.code
    code[#code + 1] = str
end

local function generate(tree, no_lines)
    local self = setmetatable({code = {}}, SELF_Class)

    if no_lines then
        self.add_line = self.ADDLINE_2
    else
        self.current = 1
        self.add_line = self.ADDLINE_1
    end

    self:emit(tree)

    if #self.code == 0 then
        return " "
    end

    return concat(self.code)
end

return generate