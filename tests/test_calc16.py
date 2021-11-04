import unittest

from calc16 import ErrorCode
from calc16 import Interpreter
from calc16 import Lexer
from calc16 import LexerError
from calc16 import Parser
from calc16 import ParserError
from calc16 import SemanticAnalyzer
from calc16 import SemanticError
from calc16 import TokenType

class TestCalc16(unittest.TestCase):
    def makeLexer(self, text):
        return Lexer(text)

    def makeParser(self, text):
        return Parser(Lexer(text))

    def runSemanticAnalyzer(self, text):
        parser = Parser(Lexer(text))
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        return semantic_analyzer

    def makeInterpreter(self, text):
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)

        interpreter = Interpreter(tree)
        return interpreter

    def test_tokens(self):
        records = (
            ('234', TokenType.V_INT, 234),
            ('3.14', TokenType.V_REAL, 3.14),
            ('+', TokenType.PLUS, '+'),
            ('-', TokenType.MINUS, '-'),
            ('*', TokenType.MUL, '*'),
            ('/', TokenType.REAL_DIV, '/'),
            ('DIV', TokenType.DIV, 'DIV'),
            ('(', TokenType.LPAREN, '('),
            (')', TokenType.RPAREN, ')'),
            (':=', TokenType.ASSIGN, ':='),
            ('.', TokenType.DOT, '.'),
            ('number', TokenType.V_VAR, 'number'),
            (';', TokenType.SEMI, ';'),
            (',', TokenType.COMMA, ','),
            (':', TokenType.COLON, ':'),
            ('BEGIN', TokenType.BEGIN, 'BEGIN'),
            ('END', TokenType.END, 'END'),
            ('PROGRAM', TokenType.PROGRAM, 'PROGRAM'),
            ('VAR', TokenType.VAR, 'VAR'),
            ('INTEGER', TokenType.INTEGER, 'INTEGER'),
            ('REAL', TokenType.REAL, 'REAL'),
            ('PROCEDURE', TokenType.PROCEDURE, 'PROCEDURE'),
        )
        for text, tok_type, tok_val in records:
            lexer = self.makeLexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_lexer_exception(self):
        lexer = self.makeLexer('<')
        with self.assertRaises(LexerError):
            lexer.get_next_token()

    def test_expression_invalid_syntax_1(self):
        parser = self.makeParser('\n'.join([
            'PROGRAM Test;',
            'VAR',
            '    a: INTEGER;',
            'BEGIN',
            '    a := 10 * ; {Invalid syntax}',
            'END.',
        ]))
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        e = cm.exception
        self.assertEqual(e.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(e.token.value, ';')
        self.assertEqual(e.token.lineno, 5)

    def test_expression_invalid_syntax_2(self):
        parser = self.makeParser('\n'.join([
            'PROGRAM Test;',
            'VAR',
            '    a: INTEGER;',
            'BEGIN',
            '    a := 1 (1 + 2); {Invalid syntax}',
            'END.',
        ]))
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        e = cm.exception
        self.assertEqual(e.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(e.token.value, '(')
        self.assertEqual(e.token.lineno, 5)

    def test_maximum_one_VAR_block_is_allowed(self):
        parser = self.makeParser('\n'.join([
            'PROGRAM Test;',
            'BEGIN',
            'END.',
        ]))
        parser.parse()

        parser = self.makeParser('\n'.join([
            'PROGRAM Test;',
            'VAR',
            '    a: INTEGER;',
            'BEGIN',
            'END.',
        ]))
        parser.parse()

        parser = self.makeParser('\n'.join([
            'PROGRAM Test;',
            'VAR',
            '    a: INTEGER;',
            'VAR',
            '    b: INTEGER;',
            'BEGIN',
            'END.',
        ]))
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        e = cm.exception
        self.assertEqual(e.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(e.token.value, 'VAR')
        self.assertEqual(e.token.lineno, 4)

    def test_semantic_duplicate_id_error(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer('\n'.join([
                'PROGRAM Test;',
                'VAR',
                '    a: INTEGER;',
                '    a: REAL;',
                'BEGIN',
                '    a := 5 + b;',
                'END.',
            ]))
        e = cm.exception
        self.assertEqual(e.error_code, ErrorCode.DUPLICATE_ID)
        self.assertEqual(e.token.value, 'a')
        self.assertEqual(e.token.lineno, 4)

    def test_semantic_id_not_found_error(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer('\n'.join([
                'PROGRAM Test;',
                'VAR',
                '    a: INTEGER;',
                'BEGIN',
                '    a := 5 + b;',
                'END.',
            ]))
        e = cm.exception
        self.assertEqual(e.error_code, ErrorCode.ID_NOT_FOUND)
        self.assertEqual(e.token.value, 'b')
        self.assertEqual(e.token.lineno, 5)

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

    def test_procedure_call(self):
        with open('data/part16.pas', 'r') as fin:
            text = fin.read()
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

    def test_program(self):
        with open('data/part15.pas', 'r') as fin:
            text = fin.read()
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        globals = interpreter.GLOBAL_MEMORY
        self.assertEqual(len(globals), 4)
        self.assertEqual(globals['number'], 2)
        self.assertEqual(globals['a'], 2)
        self.assertEqual(globals['b'], 25)
        self.assertAlmostEqual(globals['y'], float(20) / 7 + 3.14)
