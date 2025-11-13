"""
Ground Truth Python Equivalent for HELLO.cbl

Program: HELLO
Function: Display "HELLO WORLD!" message

Simple COBOL program that displays a hello world message.
"""


def hello():
    """Main procedure translated from COBOL HELLO program.
    
    Equivalent to COBOL PROCEDURE DIVISION.
    """
    print('HELLO WORLD!')
    return


if __name__ == "__main__":
    hello()
