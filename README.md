# numera
Compiler project inspired by the dragon book :dragon:. Contains only the compiler frontend at the moment.

![ci](https://github.com/dannasman/numera/actions/workflows/rust.yml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

[![Contributors](https://img.shields.io/github/contributors/dannasman/numera)](https://github.com/dannasman/numera/graphs/contributors) :joy:
## Example
Example code snippet `foo.num` in root directory:
```
define int fib(int i) {
    if (i == 0) return 1;
    if (i == 1) return 1;
    return fib(i-1)+fib(i-2);
}

define int main() {
    int[10] n; int i;
    i = 0;
    while (true) {
        if (i == 10) break;
        n[i] = fib(i);
        i = i + 1;
    }
    return 0;
}
```
Compile the code with
```
cargo run --release -- fib.num
```
Output:
```asm
.global main
fib:
	pushq %rbp
	movq %rsp, %rbp
	subq $64, %rsp
L1:
	movq 16(%rbp), %rcx
	movq $0, %rdx
	cmpq %rcx, %rdx
	sete %al
	movzx %al, %rcx
	movq %rcx, -8(%rbp)
	movq -8(%rbp), %r12
	movq %r12, %rcx
	test %rcx, %rcx
	je L3
L4:
	movq $1, %rax
	jmp L2
L3:
	movq 16(%rbp), %rcx
	movq $1, %rdx
	cmpq %rcx, %rdx
	sete %al
	movzx %al, %rcx
	movq %rcx, -16(%rbp)
	movq -16(%rbp), %r11
	movq %r11, %rcx
	test %rcx, %rcx
	je L5
L6:
	movq $1, %rax
	jmp L2
L5:
	movq 16(%rbp), %rcx
	movq $1, %rdx
	subq %rdx, %rcx
	movq %rcx, -24(%rbp)
	movq -24(%rbp), %r10
	pushq %r10
	call fib
	movq %rax, -32(%rbp)
	addq $8, %rsp
	movq 16(%rbp), %rcx
	movq $2, %rdx
	subq %rdx, %rcx
	movq %rcx, -40(%rbp)
	movq -40(%rbp), %r12
	pushq %r12
	call fib
	movq %rax, -48(%rbp)
	addq $8, %rsp
	movq -32(%rbp), %r12
	movq -48(%rbp), %r11
	movq %r12, %rcx
	movq %r11, %rdx
	addq %rdx, %rcx
	movq %rcx, -56(%rbp)
	movq -56(%rbp), %rax
	jmp L2
L2:
	addq $64, %rsp
	popq %rbp
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $112, %rsp
L7:
	movq $0, %rcx
	movq %rcx, -88(%rbp)
L9:
	movq $1, %rcx
	test %rcx, %rcx
	je L10
L11:
	movq -88(%rbp), %rcx
	movq $10, %rdx
	cmpq %rcx, %rdx
	sete %al
	movzx %al, %rcx
	movq %rcx, -96(%rbp)
	movq -96(%rbp), %r12
	movq %r12, %rcx
	test %rcx, %rcx
	je L12
L13:
	jmp L10
L12:
	movq -88(%rbp), %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -104(%rbp)
	pushq -88(%rbp)
	call fib
	movq %rax, -112(%rbp)
	addq $8, %rsp
	movq -112(%rbp), %r12
	movq -104(%rbp), %r11
	movq %r12, %rcx
	movq %rcx, -80(%rbp, %r11)
L14:
	movq -88(%rbp), %rcx
	movq $1, %rdx
	addq %rdx, %rcx
	movq %rcx, -88(%rbp)
	jmp L9
L10:
	movq $0, %rax
	jmp L8
L8:
	addq $112, %rsp
	popq %rbp
	ret
```
## Run tests
Run tests by running the following command:
```
cargo test
```
## Grammar
Grammar of the language:
```
program     ->      functions
functions   ->      functions function | ε
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
