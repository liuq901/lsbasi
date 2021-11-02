import unittest

from calc9 import ASSIGN
from calc9 import BEGIN
from calc9 import DIV
from calc9 import DOT
from calc9 import END
from calc9 import ID
from calc9 import INTEGER
from calc9 import LPAREN
from calc9 import MINUS
from calc9 import MUL
from calc9 import PLUS
from calc9 import RPAREN
from calc9 import SEMI
from calc9 import Interpreter
from calc9 import Lexer
from calc9 import Parser

class TestCalc9(unittest.TestCase):
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

    def test_lexer_new_tokens(self):
        records = (
            (':=', ASSIGN, ':='),
            ('.', DOT, '.'),
            ('number', ID, 'number'),
            (';', SEMI, ';'),
            ('BEGIN', BEGIN, 'BEGIN'),
            ('END', END, 'END'),
        )
        for text, tok_type, tok_val in records:
            lexer = Lexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_arithmetic_expression(self):
        for expr, result in (
            ('3', 3),
            ('2 + 7 * 4', 30),
            ('7 - 8 / 4', 5),
            ('14 + 2 * 3 - 6 / 2', 17),
            ('7 + 3 * (10 / (12 / (3 + 1) - 1))', 22),
            ('7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)', 10),
            ('7 + (((3 + 2)))', 12),
            ('- 3', -3),
            ('+ 3', 3),
            ('5 - - - + - 3', 8),
            ('5 - - - + - (3 + 4) - +2', 10),
        ):
            interpreter = Interpreter(Parser(Lexer(f'BEGIN a := {expr}; END.')))
            interpreter.interpret()
            globals = interpreter.GLOBAL_SCOPE
            self.assertEqual(globals['a'], result)


    def test_expression_invalid_syntax1(self):
        interpreter = Interpreter(Parser(Lexer('BEGIN a := 10 * ; END.')))
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_expression_invalid_syntax2(self):
        interpreter = Interpreter(Parser(Lexer('BEGIN a := 1 (1 + 2); END.')))
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_statement(self):
        with open('data/part9.txt', 'r') as fin:
            text = fin.read()
        interpreter = Interpreter(Parser(Lexer(text)))
        interpreter.interpret()

        globals = interpreter.GLOBAL_SCOPE
        self.assertEqual(len(globals), 5)
        self.assertEqual(globals['number'], 2)
        self.assertEqual(globals['a'], 2)
        self.assertEqual(globals['b'], 25)
        self.assertEqual(globals['c'], 27)
        self.assertEqual(globals['x'], 11)
