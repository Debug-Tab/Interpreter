import os
INTEGER = 'INTEGER'
PLUS, MINUS, MUL, DIV, MOD = 'PLUS', 'MINUS', 'MUL', 'DIV', 'MOD'
LPAREN, RPAREN = 'LPAREN', 'RPAREN'
ID, ASSIGN, BEGIN, END, SEMI, DOT, EOF = 'ID', 'ASSIGN', 'BEGIN', 'END', 'SEMI', 'DOT', 'EOF'

symbol = {
    '(': LPAREN,
    ')': RPAREN,
    '*': MUL,
    '/': DIV,
    '+': PLUS,
    '-': MINUS,
    '%': MOD,
    '=': ASSIGN,
    ';': SEMI,
    '.': DOT
}


###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################


class Token:  # 定义记号类
    def __init__(self, type, value):  # 定义构造方法
        self.type = type  # 记号中值的类型
        self.value = value  # 记号中的值

    def __str__(self):  # 重写查看记号内容的方法
        return 'Token({type},{value})'.format(
            type=self.type, value=self.value
        )

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


RESERVED_KEYWORDS = {
    'PROGRAM': Token('PROGRAM', 'PROGRAM'),
    'VAR': Token('VAR', 'VAR'),
    'INTEGER': Token('INTEGER', 'INTEGER'),
    'FLOUT': Token('FLOUT', 'FLOUT'),
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'END': Token('END', 'END'),
}


class Error:
    def __init__(self, text, pos, type, message):
        self.text = text
        self.pos = pos
        self.type = type
        self.message = message

    def output(self):
        print('{}\n{}^\n{}: {}'.format(self.text, self.pos * ' ', self.type, self.message))


class Lexer(object):
    def __init__(self, text):
        self.text = text  # 用户输入的表达式
        self.pos = 0  # 获取表达式中每一个字符时的位置
        self.line = 0
        self.line_pos = 0
        self.current_char = self.text[self.pos]

    def error(self):  # 定义提示错误的方法
        raise Exception('Error: Invalid character')  # 抛出异常

    def advance(self):  # 定义获取下一个字符的方法
        self.pos += 1  # 获取字符的位置自增
        self.line_pos += 1
        if self.pos >= len(self.text):  # 如果位置到达字符串的末尾
            self.current_char = None  # 设置当前字符为None值
        else:  # 否则
            self.current_char = self.text[self.pos]  # 设置当前字符为指定位置的字符

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos >= len(self.text):
            return None
        else:
            return self.text[peek_pos]

    def skip_space(self):  # 定义跳过空格的方法
        while self.current_char is not None and self.current_char.isspace():  # 如果当前字符不是None值并且当前字符是空格
            if self.current_char == '\n':
                self.line += 1
                self.line_pos = 0
            self.advance()  # 获取下一个字符

    def number(self):  # 获取多位数字
        value = ''
        point = 0
        while self.current_char is not None:  # 如果当前字符不是None
            if self.current_char.isdigit():
                value += self.current_char  # 连接数字
            elif self.current_char == '.':
                value += self.current_char  # 连接数字
                point += 1
            else:
                break
            self.advance()  # 获取下一个字符
        if point == 0:
            return int(value)
            # return Token('INTEGER_CONST', int(value))  # 返回数字
        elif point == 1:
            return float(value)
            # return Token('FLOUT_CONST', float(value))  # 返回数字
        else:
            print(self.line_pos)
            Error(self.text.split('\n')[self.line],
                  self.line_pos - 1,
                  'Syntax Error', 'The integer has more than one point'
                  ).output()

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()

        token = RESERVED_KEYWORDS.get(result, Token(ID, result))
        return token

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()  # the closing curly brace

    def get_next_token(self):
        while self.current_char is not None:  # 如果当前字符不是None值
            if self.current_char.isspace():  # 如果当前字符是空格
                self.skip_space()  # 跳过所有空格
                continue

            if self.current_char.isalpha():
                return self._id()

            if self.current_char.isdigit():  # 如果当前字符是整数
                return Token(INTEGER, self.number())  # 获取完整的数字创建记号对象并返回

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue
            # self.current_char == ':' and
            '''
            if self.current_char == '=':
                # self.advance()
                self.advance()
                return Token(ASSIGN, '=')

            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.')
            '''

            if self.current_char in symbol:  # 如果当前字符是符号
                temp = self.current_char
                self.advance()  # 跳到下一个字符
                return Token(symbol[temp], temp)  # 创建记号对象并返回
            print(self.line_pos)
            self.error()  # 如果以上都不是，则抛出异常。
        return Token(EOF, None)  # 遍历结束返回结束标识创建的记号对象


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################


class AST(object):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    """The Var node is constructed out of ID token."""

    def __init__(self, token):
        self.token = token
        self.value = token.value


class NoOp(AST):
    pass


class Parser:
    def __init__(self, lexer):  # 定义构造方法获取用户输入的表达式
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()  # 临时保存记号的变量

    def error(self):
        raise Exception('Invalid syntax')

    def eat(self, token_type):  # 定义辅助运算的方法，此方法用于验证记号对象的值类型是否符合运算要求。
        if self.current_token.type == token_type:  # 如果记号中的值类型符合运算要求
            self.current_token = self.lexer.get_next_token()  # 获取下一个记号对象存入变量
        else:  # 否则
            self.error()  # 抛出异常

    def factor(self):
        token = self.current_token  # 获取记号
        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == INTEGER:
            self.eat(INTEGER)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def term(self):  # 定义乘除法的语法分析与计算方法
        node = self.factor()  # 引用factor
        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == DIV:
                self.eat(DIV)
            node = BinOp(left=node, op=token, right=self.factor())
        return node

    def expr(self):
        node = self.term()  # 获取第一个整数（Term）
        while self.current_token.type in (PLUS, MINUS, MOD):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)

            elif token.type == MINUS:
                self.eat(MINUS)

            elif token.type == MOD:
                self.eat(MOD)

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def program(self):
        """program : compound_statement DOT"""
        node = self.compound_statement()
        self.eat(DOT)
        return node

    def compound_statement(self):
        """
        compound_statement : BEGIN statement_list END
        """
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)

        root = Compound()
        for node in nodes:
            root.children.append(node)
        return root

    def statement_list(self):
        """
        statement_list : statement
        | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement())

        if self.current_token.type == ID:
            self.error()

        return results

    def statement(self):
        """
        statement : compound_statement
            | assignment_statement
            | empty
        """
        if self.current_token.type == BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self):
        """
        variable : ID
        """
        node = Var(self.current_token)
        self.eat(ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def parse(self):
        """
        program : compound_statement DOT

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | assignment_statement
                  | empty

        assignment_statement : variable ASSIGN expr

        empty :

        expr: term ((PLUS | MINUS) term)*

        term: factor ((MUL | DIV) factor)*

        factor : PLUS factor
               | MINUS factor
               | INTEGER
               | LPAREN expr RPAREN
               | variable

        variable: ID
        """
        node = self.program()
        if self.current_token.type != EOF:
            self.error()

        return node


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################


class NodeVisitor:
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visitor_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):
    GLOBAL_SCOPE = {}

    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)
        elif node.op.type == MOD:
            return self.visit(node.left) % self.visit(node.right)

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        tree = self.parser.parse()
        if tree is None:
            return ''
        return self.visit(tree)

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr
