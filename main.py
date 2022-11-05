from spi import *
import sys


def main():
    while True:  # 循环获取输入
        try:
            text = input('>>>')  # 获取用户输入
        except EOFError:  # 捕获到末端错误时退出
            break
        if not text:  # 如果未输入时继续提示输入
            continue
        if len(text.strip()):
            parser = Parser(Lexer(text))
            interpreter = Interpreter(parser)
            result = interpreter.interpret()
            print(result)


def run(text):
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()

    for k, v in sorted(interpreter.GLOBAL_SCOPE.items()):
        print('{} = {}'.format(k, v))


if __name__ == '__main__':
    if len(sys.argv) == 1:
        main()
    else:
        with open(sys.argv[1],'r') as f:
            run(f.read())
