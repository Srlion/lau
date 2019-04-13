--
-- luacode-generator.lua
--
-- This file is part of the LuaJIT Language Toolkit.
--
-- Module to generate the Lua code that corresponds to a given Lua AST Tree.
-- Can be used as an alternative to the bytecode generator.

local operator = include("lau/operators.lua")

local strbyte, strsub = string.byte, string.sub

local sub = string.sub
local rep = string.rep
local find = string.find
local function SetLine(str, new_str, line)
    local len, st, en
    len, st, en = #str, 0, 0
    for i = 1, line do
        en = find(str, "\n", en + 1, true)
        if (! en) then
            if (line == i) then
                st = len - 1
            else
                str = str .. (rep("\n", line - i))
                st = len + line - i
            end
            en = st + 2
            break
        end
        if (i != line) then
            st = en
        end
    end
    return sub(str, 1, st) .. (sub(str, st + 1, en - 1) .. new_str) .. sub(str, en, len)
end

local function pack(...)
    return select("#", ...), {...}
end

local function table_to_true(tbl)
    for k, v in ipairs(tbl) do
        tbl[v] = true
        tbl[k] = nil
    end
    return tbl
end

local LuaReservedKeyword = table_to_true{
    'while', 'for', 'repeat',
    'in', 'break', 'until',
    'function', 'goto', 'if', 'else',
    'not', 'and', 'or',
    'return', 'do',
    'true', 'false', 'nil',
    -- glua
    'continue',
    -- lau
    'let', 'async', 'await', 'new'
}

local ASCII_0, ASCII_9 = 48, 57
local ASCII_a, ASCII_z = 97, 122
local ASCII_A, ASCII_Z = 65, 90

local function char_isletter(c)
    local b = strbyte(c)
    if b >= ASCII_a and b <= ASCII_z then
        return true
    elseif b >= ASCII_A and b <= ASCII_Z then
        return true
    else
        return (c == '_')
    end
end

local function char_isdigit(c)
    local b = strbyte(c)
    return b >= ASCII_0 and b <= ASCII_9
end

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

local StatementRule = { }
local ExpressionRule = { }

local concat = table.concat
local format = string.format

local function is_string(node)
    return node.kind == "Literal" and type(node.value) == "string"
end

local function is_const(node, val)
    return node.kind == "Literal" and node.value == val
end

local function is_literal(node)
    local k = node.kind
    return (k == "Literal" or k == "Table")
end

local function string_is_ident(str)
    local c = strsub(str, 1, 1)
    if c == '' or not char_isletter(c) then
        return false
    end
    for k = 2, #str do
        c = strsub(str, k, k)
        if not char_isletter(c) and not char_isdigit(c) then
            return false
        end
    end
    return not LuaReservedKeyword[str]
end

local function comma_sep_list(ls, f)
    local strls
    if f then
        strls = { }
        for k = 1, #ls do strls[k] = f(ls[k]) end
    else
        strls = ls
    end
    return concat(strls, ", ")
end

local function as_parameter(node)
    return node.kind == "Vararg" and "..." or node.name
end

function ExpressionRule:Identifier(node)
    local name = node.name
    if (node.bracketed) then
        name = "(" .. name .. ")"
    end
    self:set_line(name, node.line)
    return name, operator.ident_priority
end

function ExpressionRule:Literal(node)
    local val = node.value
    local str = type(val) == "string" and format("\"%s\"", escape(val)) or tostring(val)
    if (node.bracketed) then
        str = "(" .. str .. ")"
    end
    self:set_line(str, node.line)
    return str, operator.ident_priority
end

function ExpressionRule:MemberExpression(node)
    local _, prio = self:fake_code(function()
        return self:expr_emit(node.object)
    end)
    local bracketed
    if prio < operator.ident_priority then
        bracketed = true
    end
    self:add_symbols(bracketed, function()
        self:expr_emit(node.object)
    end)
    if node.computed then
        self:set_line("[", node.firstline)
        self:expr_emit(node.property)
        self:set_line("]", node.lastline)
    else
        self:set_line(".", node.line)
        self:expr_emit(node.property)
    end
    return "", operator.ident_priority
end

function ExpressionRule:Vararg(node)
    local str = "..."
    if (node.bracketed) then
        str = "(" .. str .. ")"
    end
    self:set_line(str, node.line)
    return str, operator.ident_priority
end

function ExpressionRule:ExpressionValue(node)
    self:add_line_2("(")
    self:expr_emit(node.value)
    self:add_line_2(")")
    return ""
end

function ExpressionRule:BinaryExpression(node)
    local oper = node.operator
    local lprio = operator.left_priority(oper)
    local rprio = operator.right_priority(oper)
    local _, alprio, arprio = self:fake_code(function()
        return self:expr_emit(node.left)
    end)
    local _, blprio, brprio = self:fake_code(function()
        return self:expr_emit(node.right)
    end)
    if not arprio then arprio = alprio end
    if not brprio then brprio = blprio end
    self:add_symbols(arprio <  lprio, function()
        self:expr_emit(node.left)
    end)
    self:set_line(oper, node.line)
    self:add_symbols(blprio <= rprio, function()
        self:expr_emit(node.right)
    end)
    return "", lprio, rprio
end

function ExpressionRule:UnaryExpression(node)
    local _, arg_prio = self:fake_code(function()
        return self:expr_emit(node.argument)
    end)
    local op_prio = operator.unary_priority
    self:set_line(node.operator, node.line)
    self:add_symbols(arg_prio < op_prio, function()
        self:expr_emit(node.argument)
    end)
    return "", operator.unary_priority
end

ExpressionRule.LogicalExpression = ExpressionRule.BinaryExpression

function ExpressionRule:ConcatenateExpression(node)
    local cat_prio = operator.left_priority("..")
    local terms = node.terms
    local terms_n = #node.terms
    for k = 1, terms_n do
        local v = terms[k]
        local _, kprio = self:fake_code(function()
            return self:expr_emit(v)
        end)
        self:add_symbols(kprio < cat_prio, function()
            self:expr_emit(v)
        end)
        if (k != terms_n) then
            local value = v.value
            self:add_line_2(" ..", v.line)
        end
    end
    return "", cat_prio
end

function ExpressionRule:Table(node)
    local last = #node.keyvals
    self:set_line("{", node.firstline)
    for i = 1, last do
        local kv = node.keyvals[i]
        local val = kv[1]
        local key = kv[2]
        if key then
            self:set_line("[", key.line)
            self:expr_emit(key)
            self:add_line_2("]=")
            self:expr_emit(val)
        else
            self:add_symbols(i == last and val.bracketed, function()
                self:expr_emit(val)
            end)
        end
        self:add_line_2(",")
    end
    if (last > 0) then
        self:remove_from_code()
    end
    self:set_line("}", node.lastline)
    return "", operator.ident_priority
end

function ExpressionRule:CallExpression(node)
    local _, prio = self:fake_code(function()
        return self:expr_emit(node.callee)
    end)
    self:add_symbols(prio < operator.ident_priority, function()
        self:expr_emit(node.callee)
    end)
    self:add_line_2("(")
    self:expr_list_2(node.arguments)
    self:add_line_2(")")
    return "", operator.ident_priority
end

function ExpressionRule:SendExpression(node)
    local _, prio = self:fake_code(function()
        return self:expr_emit(node.receiver)
    end)
    self:add_symbols(prio < operator.ident_priority, function()
        self:expr_emit(node.receiver)
    end)
    self:set_line(":", node.firstline)
    self:expr_emit(node.method)
    self:add_line_2("(")
    self:expr_list_2(node.arguments)
    self:set_line(")", node.lastline)
    return "", operator.ident_priority
end

function ExpressionRule:TenaryExpression(node)
    self:add_line_2("(")
    self:expr_emit(node.condition)
    self:add_line_2("&&")
    self:add_line_2("LAU_TENARY(")
    self:expr_emit(node.left)
    self:add_line_2(")||")
    self:add_line_2("LAU_TENARY(")
    self:expr_emit(node.right)
    self:add_line_2("))[1]")
    return "", operator.ident_priority
end

function ExpressionRule:NewExpression(node)
    local expr = node.expr
    local add_iden
    local iden
    if (expr.kind == "CallExpression") then
        iden = expr.callee.name
        add_iden = function()
            self:expr_emit(expr.callee)
        end
    else
        iden = expr.name
        add_iden = function()
            self:expr_emit(expr)
        end
    end
    self:add_line_2("(istable(")
    add_iden()
    self:add_line_2(")&&")
    add_iden()
    self:add_line_2(format([[.%s||error("can't create a new instance of '" .. type(]], iden))
    add_iden()
    self:add_line_2([[) .. "'"))(]])
    if (expr.kind == "CallExpression" and #expr.arguments > 0) then
        self:expr_list_2(expr.arguments)
    end
    self:add_line_2(")")
    return "", operator.ident_priority
end

function ExpressionRule:AwaitExpression(node)
    self:set_line("LAU_AWAIT(", node.line)
    self:expr_emit(node.expr)
    self:add_line_2(")")
    return "", operator.ident_priority
end

function StatementRule:StatementsGroup(node)
    for i = 1, #node.statements do
        self:emit(node.statements[i])
    end
end

function StatementRule:IncrementStatement(node)
    local var = node.var
    self:expr_emit(var)
    self:add_line_2("=")
    self:expr_emit(var)
    self:add_line_2(node.operator .. "1;")
end

function ExpressionRule:IncrementExpression(node)
    local var = node.var
    local add_var = function()
        self:expr_emit(var)
    end
    if node.pre then
        self:add_line_2("(function()local temp=")
        add_var()
        self:add_line_2(";")
        add_var()
        self:add_line_2("=")
        add_var()
        self:add_line_2(node.operator .. "1;return temp;end)()")
    else
        self:add_line_2("(function()")
        add_var()
        self:add_line_2("=")
        add_var()
        self:add_line_2(node.operator .. "1;return ")
        add_var()
        self:add_line_2(";end)()")
    end
    return "", operator.ident_priority
end

local function add_default_values(self, default_values)
    for k, v in ipairs(default_values) do
        self:add_line_2("if(")
        self:expr_emit(v.name)
        self:add_line_2("==nil)then ")
        self:expr_emit(v.name)
        self:add_line_2("=")
        self:expr_emit(v.default_value)
        self:add_line_2(" end ")
    end
end

function StatementRule:FunctionDeclaration(node)
    add_default_values(self, node)
    self:set_line((node.locald and "local " or "") .. "function ", node.line)
    self:expr_emit(node.id)
    local default_values = {}
    self:add_symbols(true, function()
        comma_sep_list(node.params, function(_node)
            if (_node.default_value) then
                table.insert(default_values, _node)
                _node = _node.name
            end
            self:expr_emit(_node)
            self:add_line_2(",")
        end)
        if (#node.params > 0) then
            self:remove_from_code()
        end
    end)
    add_default_values(self, default_values)
    self:add_section_2(node, node.body)
end

function StatementRule:ClassDeclaration(node)
    local class_iden = node.id.name
    self:expr_emit(node.id)
    self:add_line_2("={__class_statics = {}}")
    if (node.constructor) then
        local ast = self.ast
        local constructor = node.constructor
        table.remove(constructor.params, 1)
        local line = constructor.line
        local self_iden = ast:identifier("self", line)
        local setmeta_iden = ast:identifier("setmetatable", line)
        local args = {
            ast:expr_table({}, line, line),
            ast:identifier(class_iden, line)
        }
        local exp = ast:expr_function_call(setmeta_iden, args, line)
        table.insert(constructor.body, 1,
            ast:local_decl({self_iden}, {exp}, "=", line)
        )
        exp = ast:identifier("self", constructor.lastline)
        table.insert(constructor.body,
            ast:return_stmt({exp}, constructor.lastline)
        )
        self:emit(constructor)
    end
    self:list_emit(node.fields)
    self:list_emit(node.methods)
    self:add_line_2(
        format([[%s.__index=rawget;setmetatable(%s,{__index=%s.__class_statics,__newindex=%s.__class_statics});]], class_iden, class_iden, class_iden, class_iden)
    )
end

function StatementRule:AsyncFunctionDeclaration(node)
    StatementRule.FunctionDeclaration(self, node)
    local name = node.id.name
    self:add_line_2(name .. "=LAU_ASYNC(" .. name .. ");")
end

function ExpressionRule:FunctionExpression(node)
    self:set_line("function", node.firstline)
    local default_values = {}
    self:add_symbols(true, function()
        comma_sep_list(node.params, function(_node)
            if (_node.default_value) then
                table.insert(default_values, _node)
                _node = _node.name
            end
            self:expr_emit(_node)
            self:add_line_2(",")
        end)
        if (#node.params > 0) then
            self:remove_from_code()
        end
    end)
    add_default_values(self, default_values)
    self:add_section_2(node, node.body, true)
    return "", 0
end

function StatementRule:ForStatement(node)
    local init = node.init
    self:set_line("for ", node.firstline)
    self:expr_emit(init.id)
    self:add_line_2("=")
    self:expr_emit(init.value)
    self:add_line_2(",")
    self:expr_emit(node.last)
    if node.step and not is_const(node.step, 1) then
        self:add_line_2(",")
        self:expr_emit(node.step)
    end
    self:add_line_2(" do ")
    self:add_section_2(node, node.body)
end

function StatementRule:ForInStatement(node)
    self:set_line("for ", node.firstline)
    self:expr_list_2(node.namelist.names)
    self:add_line_2(" in ")
    self:expr_list_2(node.explist)
    self:add_line_2(" do ")
    self:add_section_2(node, node.body)
end

function StatementRule:DoStatement(node)
    self:set_line("do ", node.line)
    self:add_section_2(node, node.body)
end

function StatementRule:WhileStatement(node)
    self:set_line("while", node.firstline)
    self:expr_emit(node.test)
    self:add_line_2("do ")
    self:add_section_2(node, node.body)
end

function StatementRule:RepeatStatement(node)
    self:add_section("repeat", node.body, true)
    local test = self:expr_emit(node.test)
    local until_line = format("until %s", test)
    self:add_line(until_line)
end

function StatementRule:BreakStatement(node)
    self:set_line("break;", node.line)
end

function StatementRule:ContinueStatement(node)
    self:set_line("continue;", node.line)
end

function StatementRule:IfStatement(node)
    local ncons = #node.tests
    for i = 1, ncons do
        local header_tag = i == 1 and "if(" or "elseif("
        local v = node.tests[i]
        self:set_line(header_tag, v.firstline)
        self:expr_emit(v)
        self:set_line(")then ", v.bodystart)
        self:list_emit(node.cons[i])
    end
    if node.alternate then
        self:set_line("else ", node.alternate.line)
        self:list_emit(node.alternate)
    end
    self:set_line("end ", node.lastline)
end

function StatementRule:LocalDeclaration(node)
    self:set_line("local ", node.line)
    self:expr_list_2(node.names)
    if #node.expressions > 0 then
        self:add_line_2("=")
        if node.operator == "=" then
            self:expr_list_2(node.expressions)
        else
            self:expr_emit(node.names)
        end
    elseif (next(node.expressions)) then
        self:add_line_2("=")
        self:expr_emit(node.names[1])
        self:add_line_2(node.operator)
        self:add_line_2("(")
        self:expr_emit(node.expressions)
        self:add_line_2(")")
    end
    self:add_line_2(";")
end

function StatementRule:AssignmentExpression(node)
    if node.operator == '=' then
        self:expr_list_2(node.left)
        self:set_line("=", node.line)
        self:expr_list_2(node.right)
    else
        self:expr_emit(node.left)
        self:add_line_2("=")
        self:expr_emit(node.left)
        self:add_line_2(node.operator)
        self:add_line_2("(")
        self:expr_emit(node.right)
        self:add_line_2(")")
    end
    self:add_line_2(";")
end

function StatementRule:Chunk(node)
    self:list_emit(node.body)
end

function StatementRule:ExpressionStatement(node)
    self:expr_emit(node.expression)
    self:add_line_2(";")
end

function StatementRule:ReturnStatement(node)
    local str = "return"
    if (#node.arguments > 0) then
        str = str .. " "
    end
    self:set_line(str, node.line)
    self:expr_list_2(node.arguments)
    self:add_line_2(";")
end

function StatementRule:LabelStatement(node)
    self:set_line("::", node.line)
    self:expr_emit(node.label)
    self:add_line_2("::;")
end

function StatementRule:GotoStatement(node)
   self:set_line("goto ", node.line)
   self:expr_emit(node.label)
   self:add_line_2(";")
end

local function generate(tree, ast_builder, name)

    local self = {
        line = 0,
        code = ""
    }
    self.chunkname = tree.chunkname
    self.ast = ast_builder

    function self:add_line(line)
        local proto = self.proto
        local indent = string.rep("    ", proto.indent)
        proto.code[#proto.code + 1] = indent .. line
    end

    function self:set_line(str, line)
        self.code = SetLine(self.code, str, line)
    end

    function self:add_line_2(str)
        self.code = self.code .. str
    end

    function self:add_section_2(node, body, omit_end)
        self:list_emit(body)
        self:set_line("end", node.lastline)
        if not omit_end then
            self:add_line_2(" ")
        end
    end

    function self:expr_emit(node)
        local rule = ExpressionRule[node.kind]
        if not rule then error("cannot find an expression rule for " .. node.kind) end
        return rule(self, node)
    end

    function self:expr_list_2(exps)
        comma_sep_list(exps, function(node)
            self:expr_emit(node)
            self.code = self.code .. ","
        end)
        if (#exps > 0) then
            self:remove_from_code()
        end
    end

    function self:remove_from_code(i, i2)
        self.code = strsub(self.code, i or 1, i2 or #self.code - 1)
    end

    function self:add_symbols(condition, fn, symbol_1, symbol_2)
        if (condition) then
            self:add_line_2(symbol_1 or "(")
        end
        fn()
        if (condition) then
            self:add_line_2(symbol_2 or ")")
        end
    end

    function self:fake_code(fn)
        local old_code = self.code
        self.code = ""
        local n, args = pack(fn())
        self.code = old_code
        return unpack(args, 1, n)
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

    return self.code
end

return generate