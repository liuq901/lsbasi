import unittest

from calc4 import DIV
from calc4 import INTEGER
from calc4 import MUL
from calc4 import Interpreter
from calc4 import Lexer

class TestCalc4(unittest.TestCase):
    def test_lexer_integer(self):
        lexer = Lexer('234')
        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 234)

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
        interpreter = Interpreter(Lexer('7'))
        result = interpreter.expr()
        self.assertEqual(result, 7)

    def test_expression2(self):
        interpreter = Interpreter(Lexer('7 * 4 / 2'))
        result = interpreter.expr()
        self.assertEqual(result, 14)

    def test_expression3(self):
        interpreter = Interpreter(Lexer('7 * 4 / 2 * 3'))
        result = interpreter.expr()
        self.assertEqual(result, 42)

    def test_expression4(self):
        interpreter = Interpreter(Lexer('10 * 4 * 2 * 3 / 8'))
        result = interpreter.expr()
        self.assertEqual(result, 30)

    def test_expression_invalid_syntax(self):
        interpreter = Interpreter(Lexer('10 *'))
        with self.assertRaises(Exception):
            interpreter.expr()
