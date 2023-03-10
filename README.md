# numera
Compiler project inspired by the dragon book :dragon:. Contains only the compiler frontend at the moment.

![ci](https://github.com/dannasman/numera/actions/workflows/rust.yml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

[![Contributors](https://img.shields.io/github/contributors/dannasman/numera)](https://github.com/dannasman/numera/graphs/contributors) :joy:
## Example
Example code snippet `foo.num` in root directory:
```
{
    int x[10];
    int i = 0;
    while (true) {
        if (i == 10) break;
        x[i] = i;
        i = i + 1;
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
    int x[10];
    int i = 0;
    while (true) {
        if (i == 10) break;
        x[i] = i;
        i = i + 1;
    }
}
----------compiling----------
L1:
	i = 0
L3:
L4:
	iffalse i == 10 goto L5
L6:
	goto L2
L5:
	t1 = i * 4
	x [ t1 ] = i
L7:
	i = i + 1
	goto L3
L2:
Code compiled in 59.802µs

```
## Run tests
Run tests by running the following command:
```
cargo test
```
## Grammar
Grammar of the language:
```
program     ->      block
block       ->      { stmts }
stmts       ->      stmts stmt | ε
stmt        ->      type id = bool;
            |       type id [ num ];
            |       loc = bool;
            |       if (bool) stmt
            |       if (bool) stmt else stmt
            |       while (bool) stmt
            |       break;
            |       block
loc         ->      loc [ bool ] | id
bool        ->      bool || join | join
join        ->      join && equality | equality
equality    ->      equality == rel | equality != rel | rel
rel         ->      expr < expr | expr <= expr | expr >= expr |
                    expr > expr | expr
expr        ->      expr + term | expr - term | term
term        ->      term * unary | term / unary | unary
unary       ->      ! unary | - unary | factor
factor      ->      ( bool ) | loc | num | true | false
```
