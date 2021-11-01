import unittest

from calc6 import DIV
from calc6 import INTEGER
from calc6 import LPAREN
from calc6 import MINUS
from calc6 import MUL
from calc6 import PLUS
from calc6 import RPAREN
from calc6 import Interpreter
from calc6 import Lexer

class TestCalc6(unittest.TestCase):
    def test_lexer_integer(self):
        lexer = Lexer('234')
        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 234)

    def test_lexer_plus(self):
        lexer = Lexer('+')
        token = lexer.get_next_token()
        self.assertEqual(token.type, PLUS)
        self.assertEqual(token.value, '+')

    def test_lexer_minus(self):
        lexer = Lexer('-')
        token = lexer.get_next_token()
        self.assertEqual(token.type, MINUS)
        self.assertEqual(token.value, '-')

    def test_lexer_mul(self):
        lexer = Lexer('*')
        token = lexer.get_next_token()
        self.assertEqual(token.type, MUL)
        self.assertEqual(token.value, '*')

    def test_lexer_div(self):
        lexer = Lexer(' / ')
        token = lexer.get_next_token()
        self.assertEqual(token.type, DIV)
        self.assertEqual(token.value, '/')

    def test_lexer_lparen(self):
        lexer = Lexer('(')
        token = lexer.get_next_token()
        self.assertEqual(token.type, LPAREN)
        self.assertEqual(token.value, '(')

    def test_lexer_rparen(self):
        lexer = Lexer(')')
        token = lexer.get_next_token()
        self.assertEqual(token.type, RPAREN)
        self.assertEqual(token.value, ')')

    def test_expression1(self):
        interpreter = Interpreter(Lexer('3'))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 3)

    def test_expression2(self):
        interpreter = Interpreter(Lexer('2 + 7 * 4'))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 30)

    def test_expression3(self):
        interpreter = Interpreter(Lexer('7 - 8 / 4'))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 5)

    def test_expression4(self):
        interpreter = Interpreter(Lexer('14 + 2 * 3 - 6 / 2'))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 17)

    def test_expression5(self):
        interpreter = Interpreter(Lexer(
            '7 + 3 * (10 / (12 / (3 + 1) - 1))'
        ))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 22)

    def test_expression6(self):
        interpreter = Interpreter(Lexer(
            '7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)'
        ))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 10)

    def test_expression7(self):
        interpreter = Interpreter(Lexer('7 + (((3 + 2)))'))
        result = interpreter.expr(outmost=True)
        self.assertEqual(result, 12)

    def test_expression_invalid_syntax(self):
        interpreter = Interpreter(Lexer('10 *'))
        with self.assertRaises(Exception):
            interpreter.expr(outmost=True)
