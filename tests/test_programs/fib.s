.global main
fib:
	pushq %rbp
	movq %rsp, %rbp
	subq $64, %rsp
L1:
	movq 16(%rbp), %rdx
	movq $0, %rcx
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
	movq 16(%rbp), %rdx
	movq $1, %rcx
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
	movq -88(%rbp), %rdx
	movq $10, %rcx
	cmpq %rcx, %rdx
	setl %al
	movzx %al, %rcx
	movq %rcx, -96(%rbp)
	movq -96(%rbp), %r12
	movq %r12, %rcx
	test %rcx, %rcx
	je L10
L11:
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
L12:
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

