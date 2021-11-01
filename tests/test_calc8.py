import unittest

from calc8 import DIV
from calc8 import INTEGER
from calc8 import LPAREN
from calc8 import MINUS
from calc8 import MUL
from calc8 import PLUS
from calc8 import RPAREN
from calc8 import Interpreter
from calc8 import Lexer
from calc8 import Parser

class TestCalc8(unittest.TestCase):
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
        interpreter = Interpreter(Parser(Lexer('3')))
        result = interpreter.interpret()
        self.assertEqual(result, 3)

    def test_expression2(self):
        interpreter = Interpreter(Parser(Lexer('2 + 7 * 4')))
        result = interpreter.interpret()
        self.assertEqual(result, 30)

    def test_expression3(self):
        interpreter = Interpreter(Parser(Lexer('7 - 8 / 4')))
        result = interpreter.interpret()
        self.assertEqual(result, 5)

    def test_expression4(self):
        interpreter = Interpreter(Parser(Lexer('14 + 2 * 3 - 6 / 2')))
        result = interpreter.interpret()
        self.assertEqual(result, 17)

    def test_expression5(self):
        interpreter = Interpreter(Parser(Lexer(
            '7 + 3 * (10 / (12 / (3 + 1) - 1))'
        )))
        result = interpreter.interpret()
        self.assertEqual(result, 22)

    def test_expression6(self):
        interpreter = Interpreter(Parser(Lexer(
            '7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)'
        )))
        result = interpreter.interpret()
        self.assertEqual(result, 10)

    def test_expression7(self):
        interpreter = Interpreter(Parser(Lexer('7 + (((3 + 2)))')))
        result = interpreter.interpret()
        self.assertEqual(result, 12)

    def test_expression8(self):
        interpreter = Interpreter(Parser(Lexer('- 3')))
        result = interpreter.interpret()
        self.assertEqual(result, -3)

    def test_expression9(self):
        interpreter = Interpreter(Parser(Lexer('+ 3')))
        result = interpreter.interpret()
        self.assertEqual(result, 3)

    def test_expression10(self):
        interpreter = Interpreter(Parser(Lexer('5 - - - + - 3')))
        result = interpreter.interpret()
        self.assertEqual(result, 8)

    def test_expression11(self):
        interpreter = Interpreter(Parser(Lexer('5 - - - + - (3 + 4) - + 2')))
        result = interpreter.interpret()
        self.assertEqual(result, 10)

    def test_no_expression(self):
        interpreter = Interpreter(Parser(Lexer('   ')))
        result = interpreter.interpret()
        self.assertEqual(result, '')

    def test_expression_invalid_syntax1(self):
        interpreter = Interpreter(Parser(Lexer('10 *')))
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_expression_invalid_syntax2(self):
        interpreter = Interpreter(Parser(Lexer('1 (1 + 2')))
        with self.assertRaises(Exception):
            interpreter.interpret()
