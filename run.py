from spi import *
from sys import argv
with open(argv[1],'r') as f:
    lexer = Lexer(f.read)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)
