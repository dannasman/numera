# numera
Compiler project inspired by the dragon book. Contains only the compiler frontend at the moment.
## Example
Example code snippet `foo.num` in root directory:
```
{
    x = 1;
    while(true) {
        if (x == 5) break;
        x = x + 1;
    }
}
```
Compile the code with
```
cargo run foo.txt
```
Output:
```
{
    int x = 1;
    while(true) {
        if (x == 5) break;
        x = x + 1;
    }
}

----------compiling----------
L1:
	x = 1
L3:
L4:
	iffalse x == 5 goto L5
L6:
	goto L2
L5:
	x = x + 1
	goto L3
L2:
Code compiled in 96.979µs
```
## Grammar
Grammar of the language:
```
program     ->      block
block       ->      { stmts }
stmts       ->      stmts stmt | ε
stmt        ->      type id = bool;
            |       id = bool;
            |       if (bool) stmt
            |       if (bool) stmt else stmt
            |       while (bool) stmt
            |       break;
            |       block
bool        ->      bool || join | join
join        ->      join && equality | equality
equality    ->      equality == rel | equality != rel | rel
rel         ->      expr < expr | expr <= expr | expr >= expr |
                    expr > expr | expr
expr        ->      expr + term | expr - term | term
term        ->      term * unary | term / unary | unary
unary       ->      ! unary | - unary | factor
factor      ->      ( bool ) | id | num | true | false
```
