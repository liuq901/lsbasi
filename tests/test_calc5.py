import unittest

from calc5 import DIV
from calc5 import INTEGER
from calc5 import MINUS
from calc5 import MUL
from calc5 import PLUS
from calc5 import Interpreter
from calc5 import Lexer

class TestCalc5(unittest.TestCase):
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

    def test_expression1(self):
        interpreter = Interpreter(Lexer('3'))
        result = interpreter.expr()
        self.assertEqual(result, 3)

    def test_expression2(self):
        interpreter = Interpreter(Lexer('2 + 7 * 4'))
        result = interpreter.expr()
        self.assertEqual(result, 30)

    def test_expression3(self):
        interpreter = Interpreter(Lexer('7 - 8 / 4'))
        result = interpreter.expr()
        self.assertEqual(result, 5)

    def test_expression4(self):
        interpreter = Interpreter(Lexer('14 + 2 * 3 - 6 / 2'))
        result = interpreter.expr()
        self.assertEqual(result, 17)

    def test_expression_invalid_syntax(self):
        interpreter = Interpreter(Lexer('10 *'))
        with self.assertRaises(Exception):
            interpreter.expr()
