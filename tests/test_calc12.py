import unittest

from calc12 import ASSIGN
from calc12 import BEGIN
from calc12 import COLON
from calc12 import COMMA
from calc12 import DIV
from calc12 import DOT
from calc12 import END
from calc12 import ID
from calc12 import INTEGER
from calc12 import LPAREN
from calc12 import MINUS
from calc12 import MUL
from calc12 import PLUS
from calc12 import PROC
from calc12 import PROGRAM
from calc12 import REAL
from calc12 import REAL_DIV
from calc12 import RPAREN
from calc12 import SEMI
from calc12 import T_INT
from calc12 import T_REAL
from calc12 import VAR
from calc12 import Interpreter
from calc12 import Lexer
from calc12 import Parser
from calc12 import SymbolTableBuilder

class TestCalc12(unittest.TestCase):
    def makeInterpreter(self, text):
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        symtal_builder = SymbolTableBuilder(output=False)
        symtal_builder.visit(tree)

        interpreter = Interpreter(tree)
        return interpreter

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
            ('INTEGER', T_INT, 'INTEGER'),
            ('REAL', T_REAL, 'REAL'),
            ('PROCEDURE', PROC, 'PROCEDURE'),
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
            interpreter = self.makeInterpreter('\n'.join([
                'PROGRAM Test;',
                'VAR',
                '    a: INTEGER;',
                'BEGIN',
                '    a := {}'.format(expr),
                'END.'
            ]))
            interpreter.interpret()
            globals = interpreter.GLOBAL_MEMORY
            self.assertEqual(globals['a'], result)

    def test_float_arithmetic_expression(self):
        for expr, result in (
            ('3.14', 3.14),
            ('2.14 + 7 * 4', 30.14),
            ('7.14 - 8 / 4', 5.14),
        ):
            interpreter = self.makeInterpreter('\n'.join([
                'PROGRAM Test;',
                'VAR',
                '    a: REAL;',
                'BEGIN',
                '    a := {}'.format(expr),
                'END.'
            ]))
            interpreter.interpret()
            globals = interpreter.GLOBAL_MEMORY
            self.assertEqual(globals['a'], result)

    def test_expression_invalid_syntax1(self):
        with self.assertRaises(Exception):
            interpreter = self.makeInterpreter('\n'.join([
                'PROGRAM Test;',
                'BEGIN',
                '    a := 10 *; {Invalid syntax}',
                'END.',
            ]))

    def test_expression_invalid_syntax2(self):
        with self.assertRaises(Exception):
            interpreter = self.makeInterpreter('\n'.join([
                'PROGRAM Test;',
                'BEGIN',
                '    a := 1 (1 + 2); {Invalid syntax}',
                'END.',
            ]))

    def test_program(self):
        with open('data/part12.pas', 'r') as fin:
            text = fin.read()
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        globals = interpreter.GLOBAL_MEMORY
        self.assertEqual(len(globals), 1)
        self.assertEqual(globals['a'], 10)
