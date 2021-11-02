import unittest

from calc10 import ASSIGN
from calc10 import BEGIN
from calc10 import COLON
from calc10 import COMMA
from calc10 import DIV
from calc10 import DOT
from calc10 import END
from calc10 import ID
from calc10 import INTEGER
from calc10 import INTEGER_TYPE
from calc10 import LPAREN
from calc10 import MINUS
from calc10 import MUL
from calc10 import PLUS
from calc10 import PROGRAM
from calc10 import REAL
from calc10 import REAL_DIV
from calc10 import REAL_TYPE
from calc10 import RPAREN
from calc10 import SEMI
from calc10 import VAR
from calc10 import Interpreter
from calc10 import Lexer
from calc10 import Parser

class TestCalc10(unittest.TestCase):
    def test_tokens(self):
        records = (
            ('234', INTEGER, 234),
            ('3.14', REAL, 3.14),
            ('+', PLUS, '+'),
            ('-', MINUS, '-'),
            ('*', MUL, '*'),
            ('/', REAL_DIV, '/'),
            ('DIV', DIV, '/'),
            ('(', LPAREN, '('),
            (')', RPAREN, ')'),
            (':=', ASSIGN, ':='),
            ('.', DOT, '.'),
            ('number', ID, 'number'),
            (';', SEMI, ';'),
            (',', COMMA, ','),
            (':', COLON, ':'),
            ('BEGIN', BEGIN, 'BEGIN'),
            ('END', END, 'END'),
            ('PROGRAM', PROGRAM, 'PROGRAM'),
            ('VAR', VAR, 'VAR'),
            ('INTEGER', INTEGER_TYPE, 'INTEGER'),
            ('REAL', REAL_TYPE, 'REAL')
        )
        for text, tok_type, tok_val in records:
            lexer = Lexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_integer_arithmetic_expression(self):
        for expr, result in (
            ('3', 3),
            ('2 + 7 * 4', 30),
            ('7 - 8 DIV 4', 5),
            ('14 + 2 * 3 - 6 DIV 2', 17),
            ('7 + 3 * (10 DIV (12 DIV (3 + 1) - 1))', 22),
            ('7 + 3 * (10 DIV (12 DIV (3 + 1) - 1)) DIV (2 + 3) - 5 - 3 + (8)', 10),
            ('7 + (((3 + 2)))', 12),
            ('- 3', -3),
            ('+ 3', 3),
            ('5 - - - + - 3', 8),
            ('5 - - - + - (3 + 4) - +2', 10),
        ):
            interpreter = Interpreter(Parser(Lexer('\n'.join([
                'PROGRAM Test;',
                'VAR',
                '    a: INTEGER;',
                'BEGIN',
                '    a := {}'.format(expr),
                'END.'
            ]))))
            interpreter.interpret()
            globals = interpreter.GLOBAL_SCOPE
            self.assertEqual(globals['a'], result)

    def test_float_arithmetic_expression(self):
        for expr, result in (
            ('3.14', 3.14),
            ('2.14 + 7 * 4', 30.14),
            ('7.14 - 8 / 4', 5.14),
        ):
            interpreter = Interpreter(Parser(Lexer('\n'.join([
                'PROGRAM Test;',
                'VAR',
                '    a: REAL;',
                'BEGIN',
                '    a := {}'.format(expr),
                'END.'
            ]))))
            interpreter.interpret()
            globals = interpreter.GLOBAL_SCOPE
            self.assertEqual(globals['a'], result)

    def test_expression_invalid_syntax1(self):
        interpreter = Interpreter(Parser(Lexer('\n'.join([
            'PROGRAM Test;',
            'BEGIN',
            '    a := 10 *; {Invalid syntax}',
            'END.',
        ]))))
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_expression_invalid_syntax2(self):
        interpreter = Interpreter(Parser(Lexer('\n'.join([
            'PROGRAM Test;',
            'BEGIN',
            '    a := 1 (1 + 2); {Invalid syntax}',
            'END.',
        ]))))
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_program(self):
        with open('data/part10.pas', 'r') as fin:
            text = fin.read()
        interpreter = Interpreter(Parser(Lexer(text)))
        interpreter.interpret()

        globals = interpreter.GLOBAL_SCOPE
        self.assertEqual(len(globals), 6)
        self.assertEqual(globals['number'], 2)
        self.assertEqual(globals['a'], 2)
        self.assertEqual(globals['b'], 25)
        self.assertEqual(globals['c'], 27)
        self.assertEqual(globals['x'], 11)
        self.assertAlmostEqual(globals['y'], float(20) / 7 + 3.14)
