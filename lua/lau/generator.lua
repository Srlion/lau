local lexer   = Lau.lexer
local Keyword = lexer.Keyword
local Literal = lexer.Literal
local Op       = lexer.Op
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

local function comma_sep_list(self, t, f)
    local n = #t
    for i = 1, n do
        f(t[i])

        if i ~= n then
            self:add_line(",")
        end
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
            continue
        end

        self:add_line("if ")

        if type then
            type = type.value

            local is_type = is_types[type:lower()]
            if is_type then
                self:add_line("not " .. is_type .. "(")
                self:expr_emit(param)
                self:add_line(")")
                type = is_type:sub(3)
            else
                type = types_to_replace[type:lower()] or type
                self:add_line("type(")
                self:expr_emit(param)
                self:add_line(")~=\"" .. type .. "\"")
            end

            self:add_line(" then ")

            if value then
                goto VALUE
            end

            self:add_line(format(bad_argument_message, i, func_name:gsub("%.", ""), type, param.value))
        else
            self:expr_emit(param)
            self:add_line("==")
            self:add_line("nil")
            self:add_line(" then ")
        end

        ::VALUE::

        if value then
            self:expr_emit(param)
            self:add_line("=")
            self:expr_emit(value)
        end

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
    "end", "then", "elseif", "repeat", "until", "not", "or", "function"
})

local function should_replace(v)
    local value = v.value

    if ReplacedKeyword[value] then
        v.value = "\"" .. value .. "\""
        v.no_change = true
        return true
    end

    return false
end

local function get_name(name)
    for k, v in ipairs(Lau.Modules) do
        if v.name == name then
            return v[1]
        end
    end
end

local StatementRule = {}
local ExpressionRule = {}

function StatementRule:Chunk(node)
    self:list_emit(node.body)
end

function StatementRule:Text(node)
    self:add_line(node.text, node.line)
end
ExpressionRule.Text = StatementRule.Text

function StatementRule:FunctionDeclaration(node)
    local id = node.id

    if node.locald then
        self:add_line("local ")
        self:expr_emit(id)
        self:add_line(";")
    end

    self:expr_emit(id)
    self:add_line("=")

    if node.is_async then
        self:add_line(get_name("async") .. "(")
    end

    self:add_line("function(")
    self:expr_list(node.params)
    self:add_line(")")

    local func_name
    if id.kind == "MemberExpression" then
        func_name = id.property.value
    else
        func_name = id.value
    end

    check_params(self, node.params, func_name)

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

    --[[
        class_name = class_name or {
            class = {}
        }

        local class_meta = {
            __index = class_name.class
        }
    ]]

    self:expr_emit(node.name)
    self:add_line("=")
    self:expr_emit(node.name)
    self:add_line(" or ")
    self:add_line("{class={}};")
    self:add_line("local class_meta={__index=")
    self:expr_emit(node.name)
    self:add_line(".class};")

    local node_body = node.body
    for i = 1, #node_body do
        local v = node_body[i]

        local is_static = v.is_static

        local id = v.field and v or v.id
        local name = id.value

        if name == "tostring" and not v.ctor and not v.field then
            id.value = "__tostring"

            if not is_static then
                self:add_line("class_meta.")
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

        if not v.ctor and not is_static then
            self:add_line(".class")
        end

        if should_replace(id) then
            id.value = "[" .. id.value .. "]"
        else
            id.value = "." .. name
        end

        if v.ctor then
            v.params[1].value = "Self"
            local body = v.body

            table.insert(body, 1, {
                kind = "Text",
                text = "local self=setmetatable({}, class_meta);"
            })

            local last_stmt = body[#body]

            if not last_stmt or last_stmt.kind ~= "ReturnStatement" then
                table.insert(body, {
                    kind = "Text",
                    text = "return self;"
                })
            end

            self:emit(v)
        elseif v.field then
            self:expr_emit(v)
            self:add_line("=")
            self:expr_emit(v.body)
            self:add_line(";")
        else
            if not is_static then
                local body = v.body
                local last_stmt = body[#body]

                if not last_stmt or last_stmt.kind ~= "ReturnStatement" then
                    table.insert(body, {
                        kind = "Text",
                        text = "return self;"
                    })
                end
            else
                table.remove(v.params, 1)
            end

            self:emit(v)
        end
    end

    self:add_line("end;")
end

function StatementRule:UseStatement(node)
    local uses = node.uses

    self:add_line("local ")

    comma_sep_list(self, uses, function(v)
        local locals = v.locals
        if locals then
            self:expr_list(locals)
        else
            self:expr_emit(v.ident)
        end
    end)

    self:add_line("=")

    comma_sep_list(self, uses, function(v)
        local ident  = v.ident
        local locals = v.locals
        if locals then
            comma_sep_list(self, locals, function(v)
                self:expr_emit(ident)
                if should_replace(v) then
                    self:add_line("[")
                    self:expr_emit(v)
                    self:add_line("]")
                else
                    self:add_line(".")
                    self:expr_emit(v)
                end
            end)
        else
            self:expr_emit(ident)
        end
    end)

    self:add_line(";")
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
    self:add_line("until not(")
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


-- local loops_n = 0
-- local function get_label(k)
--     return k .. "_" .. loops_n
-- end
-- function StatementRule:ForEachStatement(node)
--     do
--         local first_exp = node.exps[1]
--         if first_exp.kind == "CallExpression" and first_exp.callee.value == "ipairs" and #node.vars == 2 then
--             -- do
--             --     local i, table_n = 0, #table
--             --     ::__for_begin__::
--             --     i = i + 1;
--             --     if i <= table_n then
--             --          local v, i, table_n = table[i], i;
--             --          if v == nil then
--             --              goto __for_break__;
--             --          end

--             --          <body>

--             --          goto __for_begin__;
--             --     end
--             --     ::__for_break__::
--             -- end

--             self:add_line("do ")
--             loops_n = loops_n + 1

--             local LOOP, BREAK = get_label("__for_begin__"), get_label("__for_break__")

--             local table_name; do
--                 local t = first_exp.arguments[1]
--                 if t.kind == "Identifier" then
--                     table_name = {
--                         kind = "Text",
--                         text = t.value
--                     }
--                 else
--                     table_name = {
--                         kind = "Text",
--                         text = "__t__"
--                     }
--                     self:add_line("local __t__=")
--                     self:expr_emit(t)
--                     self:add_line(";")
--                 end
--             end

--             local key, value = key, node.vars[2]

--             self:add_line("local")
--             self:expr_emit(node.vars[1])
--             self:add_line(",__t__n=0,#")
--             self:expr_emit(table_name)

--             self:add_line("::" .. LOOP .. "::")

--             self:expr_emit(key)
--             self:add_line("=")
--             self:expr_emit(key)
--             self:add_line("+1;")

--             self:add_line("if ")
--             self:expr_emit(key)
--             self:add_line("<=__t__n then")

--             self:add_line("local ")
--             self:expr_emit(value)
--             self:add_line(",")
--             self:expr_emit(key)
--             self:add_line(",")
--             self:add_line("__t__n")
--             self:add_line("=")
--             self:expr_emit(table_name)
--             self:add_line("[")
--             self:expr_emit(key)
--             self:add_line("]")
--             self:add_line(",")
--             self:expr_emit(key)
--             self:add_line(";")

--             self:add_line("if ")
--             self:expr_emit(value)
--             self:add_line("==")
--             self:add_line("nil")
--             self:add_line(" then ")
--             self:add_line("goto ")
--             self:add_line(BREAK)
--             self:add_line(";")
--             self:add_line("end;")

--             self:list_emit(node.body)

--             self:add_line("goto ")
--             self:add_line(LOOP)
--             self:add_line(";")

--             self:add_line("end")
--             self:add_line(";")

--             loops_n = loops_n - 1

--             return
--         end
--     end

--     self:add_line("for ")
--     self:expr_list(node.vars)
--     self:add_line(" in ")
--     self:expr_list(node.exps)
--     self:add_line(" do ")
--     self:add_section(node.body)
-- end

function StatementRule:ForEachStatement(node)
    do
        local first_exp = node.exps[1]
        if first_exp.kind == "CallExpression" and first_exp.callee.value == "ipairs" and #node.vars == 2 then
            local table_name, in_do; do
                local t = first_exp.arguments[1]
                if t.kind == "Identifier" then
                    table_name = {
                        kind = "Text",
                        text = t.value
                    }
                else
                    table_name = {
                        kind = "Text",
                        text = "__t__"
                    }
                    in_do = true
                end
            end

            local key, value = node.vars[1], node.vars[2]

            if in_do then
                self:add_line("do local __t__=")
                self:expr_emit(first_exp.arguments[1])
                self:add_line(";")
            end

            self:add_line("for ")
            self:expr_emit(node.vars[1])
            self:add_line("=1,#")
            self:expr_emit(table_name)
            self:add_line(" do local ")
            self:expr_emit(value)
            self:add_line("=")
            self:expr_emit(table_name)
            self:add_line("[")
            self:expr_emit(key)
            self:add_line("]")
            self:add_line(";if ")
            self:expr_emit(value)
            self:add_line("==nil then break;end;")
            self:add_section(node.body)

            if in_do then
                self:add_line("end;")
            end

            return
        end
    end

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
    self:add_line("::", node.label.line)
    self:expr_emit(node.label)
    self:add_line("::")
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
    local v = node.value
    if not node.no_change and ReplacedKeyword[v] then
        v = "​" --[[invisible character]] .. v
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
    self:expr_list(node.params)
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
        self:add_line("[")
        self:expr_emit(prop)
        self:add_line("]")
    else
        self:add_line(".")
        self:expr_emit(prop)
    end
end

function ExpressionRule:Vararg(node)
    self:add_line("...", node.line)
end

function ExpressionRule:BinaryExpression(node)
    local oper = node.operator
    if oper == Op.Ne then
        oper = "~="
    elseif oper == Op.LAnd then
        oper = " and "
    elseif oper == Op.LOr then
        oper = " or "
    else
        oper = tostring(oper)
        oper = strsub(oper, 2, #oper - 1)
    end

    self:expr_emit(node.left)
    self:add_line(oper)
    self:expr_emit(node.right)
end
ExpressionRule.LogicalExpression = ExpressionRule.BinaryExpression

function ExpressionRule:UnaryExpression(node)
    local oper = node.operator
    if oper == Op.Not then
        oper = " not "
    else
        oper = tostring(oper)
        oper = strsub(oper, 2, #oper - 1)
    end

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
            if should_replace(key) or key.bracketed or key.key ~= "Ident" then
                self:add_line("[")
                self:expr_emit(key)
                self:add_line("]")
            elseif key.key == "Ident" and not key.bracketed then
                self:expr_emit(key)
            end
            self:add_line("=")
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
        self:add_line(",")
        self:expr_emit(method)

        if args then
            self:add_line(",")
        end
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
    comma_sep_list(self, exps, function(v)
        self:expr_emit(v)
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

    local code = concat(self.code)
    code = code:gsub("[ \t]+%f[\r\n%z]", "") // remove leading whitespaces
    code = code:gsub(" +", " ") // remove multiple spaces

    return code
end

return generate