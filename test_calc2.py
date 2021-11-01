import unittest

from calc2 import EOF
from calc2 import INTEGER
from calc2 import MINUS
from calc2 import PLUS
from calc2 import Interpreter

class TestCalc2(unittest.TestCase):
    def test_lexer_integer(self):
        lexer = Interpreter('234')
        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 234)

    def test_lexer_plus(self):
        lexer = Interpreter('+')
        token = lexer.get_next_token()
        self.assertEqual(token.type, PLUS)
        self.assertEqual(token.value, '+')

    def test_lexer_minus(self):
        lexer = Interpreter('-')
        token = lexer.get_next_token()
        self.assertEqual(token.type, MINUS)
        self.assertEqual(token.value, '-')

    def test_lexer_eof(self):
        lexer = Interpreter('-')
        token = lexer.get_next_token()
        token = lexer.get_next_token()
        self.assertEqual(token.type, EOF)

    def test_lexer_whitespace(self):
        lexer = Interpreter('  23')
        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 23)

    def test_lexer_addition(self):
        lexer = Interpreter('2+3')

        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 2)

        token = lexer.get_next_token()
        self.assertEqual(token.type, PLUS)
        self.assertEqual(token.value, '+')

        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 3)

        token = lexer.get_next_token()
        self.assertEqual(token.type, EOF)

    def test_lexer_subtraction(self):
        lexer = Interpreter(' 27   -  7  ')

        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 27)

        token = lexer.get_next_token()
        self.assertEqual(token.type, MINUS)
        self.assertEqual(token.value, '-')

        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 7)

        token = lexer.get_next_token()
        self.assertEqual(token.type, EOF)

    def test_interpreter_addition(self):
        interpreter = Interpreter(' 23 + 7')
        result = interpreter.expr()
        self.assertEqual(result, 30)

    def test_interpreter_subtraction(self):
        interpreter = Interpreter(' 27   -  7  ')
        result = interpreter.expr()
        self.assertEqual(result, 20)

if __name__ == '__main__':
    unittest.main()
