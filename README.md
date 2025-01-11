# numera
Compiler project inspired by the dragon book :dragon:. Contains only the compiler frontend at the moment.

![ci](https://github.com/dannasman/numera/actions/workflows/rust.yml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

[![Contributors](https://img.shields.io/github/contributors/dannasman/numera)](https://github.com/dannasman/numera/graphs/contributors) :joy:
## Example
Example code snippet `foo.num` in root directory:
```
{
    def int fib(int n) {
        if (n == 0) {
            return 1;
        }

        if (n == 1) {
            return 1;
        }

        return fib(n - 1) + fib(n - 2);
    }

    int n[10];
    int i = 0;
    while (i < 10) {
        n[i] = fib(i);
    }
}
```
Compile the code with
```
cargo run --release foo.num
```
Output:
```
{
    def int fib(int n) {
        if (n == 0) {
            return 1;
        }

        if (n == 1) {
            return 1;
        }

        return fib(n - 1) + fib(n - 2);
    }

    int n[10];
    int i = 0;
    while (i < 10) {
        n[i] = fib(i);
    }
}

----------compiling----------
fib:
POP n
EQ t1 n 0
GOTO L3 iffalse t1
L4:
PUSH 1
RET
L3:
EQ t2 n 1
GOTO L5 iffalse t2
L6:
PUSH 1
RET
L5:
SUB t4 n 1
PUSH t4
CALL t5 fib
SUB t6 n 2
PUSH t6
CALL t7 fib
ADD t3 t5 t7
PUSH t3
RET
L1:
i 0
L7:
LT t8 i 10
GOTO L2 iffalse t8
L8:
MUL t9 i 4
PUSH i
CALL n [ t9 ] fib
GOTO L7
L2:
Code compiled in 82.338µs
```
## Run tests
Run tests by running the following command:
```
cargo test
```
## Grammar
Grammar of the language:
```
program     ->      program function | ε
function    ->      define type id ( params) block
block       ->      { stmts }
stmts       ->      stmts stmt | ε
stmt        ->      type id = bool;
            |       type id [ num ];
            |       loc = bool;
            |       if ( bool ) stmt
            |       if ( bool ) stmt else stmt
            |       while ( bool ) stmt
            |       return expr;
            |       break;
            |       block
params      |       ε | type id, params
args        |       ε | id, args
loc         ->      loc [ bool ] | id | id ( args )
bool        ->      bool || join | join
join        ->      join && equality | equality
equality    ->      equality == rel | equality != rel | rel
rel         ->      expr < expr | expr <= expr | expr >= expr |
                    expr > expr | expr
expr        ->      expr + term | expr - term | term
term        ->      term * unary | term / unary | unary
unary       ->      ! unary | - unary | factor
factor      ->      ( bool ) | loc | num | real | true | false
```
