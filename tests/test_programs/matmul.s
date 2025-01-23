.global main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $6416, %rsp
L1:
	movq $0, %rcx
	movq %rcx, -6152(%rbp)
L3:
	movq -6152(%rbp), %rdx
	movq $16, %rcx
	cmpq %rcx, %rdx
	setl %al
	movzx %al, %rcx
	movq %rcx, -6176(%rbp)
	movq -6176(%rbp), %r12
	movq %r12, %rcx
	test %rcx, %rcx
	je L4
L5:
	movq $0, %rcx
	movq %rcx, -6160(%rbp)
L6:
	movq -6160(%rbp), %rdx
	movq $16, %rcx
	cmpq %rcx, %rdx
	setl %al
	movzx %al, %rcx
	movq %rcx, -6184(%rbp)
	movq -6184(%rbp), %r11
	movq %r11, %rcx
	test %rcx, %rcx
	je L7
L8:
	movq -6152(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6192(%rbp)
	movq -6192(%rbp), %r10
	movq %r10, %rcx
	movq -6160(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6200(%rbp)
	movq -6200(%rbp), %r9
	movq %r9, %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6208(%rbp)
	movq -6208(%rbp), %r8
	movq -6152(%rbp), %rcx
	movq %rcx, -2048(%rbp, %r8)
L9:
	movq -6152(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6216(%rbp)
	movq -6216(%rbp), %rdi
	movq %rdi, %rcx
	movq -6160(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6224(%rbp)
	movq -6224(%rbp), %rsi
	movq %rsi, %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6232(%rbp)
	movq -6232(%rbp), %rax
	movq -6160(%rbp), %rcx
	movq %rcx, -4096(%rbp, %rax)
L10:
	movq -6152(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6240(%rbp)
	movq -6240(%rbp), %rcx
	movq -6160(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6248(%rbp)
	movq -6248(%rbp), %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6256(%rbp)
	movq -6256(%rbp), %rax
	movq $0, %rcx
	movq %rcx, -6144(%rbp, %rax)
L11:
	movq -6160(%rbp), %rcx
	movq $1, %rdx
	addq %rdx, %rcx
	movq %rcx, -6160(%rbp)
	jmp L6
L7:
	movq -6152(%rbp), %rcx
	movq $1, %rdx
	addq %rdx, %rcx
	movq %rcx, -6152(%rbp)
	jmp L3
L4:
	movq $0, %rcx
	movq %rcx, -6152(%rbp)
L12:
	movq -6152(%rbp), %rdx
	movq $16, %rcx
	cmpq %rcx, %rdx
	setl %al
	movzx %al, %rcx
	movq %rcx, -6264(%rbp)
	movq -6264(%rbp), %rcx
	test %rcx, %rcx
	je L13
L14:
	movq $0, %rcx
	movq %rcx, -6160(%rbp)
L15:
	movq -6160(%rbp), %rdx
	movq $16, %rcx
	cmpq %rcx, %rdx
	setl %al
	movzx %al, %rcx
	movq %rcx, -6272(%rbp)
	movq -6272(%rbp), %rcx
	test %rcx, %rcx
	je L16
L17:
	movq $0, %rcx
	movq %rcx, -6168(%rbp)
L18:
	movq -6168(%rbp), %rdx
	movq $16, %rcx
	cmpq %rcx, %rdx
	setl %al
	movzx %al, %rcx
	movq %rcx, -6280(%rbp)
	movq -6280(%rbp), %rcx
	test %rcx, %rcx
	je L19
L20:
	movq -6152(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6288(%rbp)
	movq -6288(%rbp), %rcx
	movq -6160(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6296(%rbp)
	movq -6296(%rbp), %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6304(%rbp)
	movq -6152(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6312(%rbp)
	movq -6312(%rbp), %rcx
	movq -6160(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6320(%rbp)
	movq -6320(%rbp), %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6328(%rbp)
	movq -6328(%rbp), %rax
	movq -6144(%rbp, %rax), %rcx
	movq %rcx, -6336(%rbp)
	movq -6152(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6344(%rbp)
	movq -6344(%rbp), %rcx
	movq -6168(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6352(%rbp)
	movq -6352(%rbp), %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6360(%rbp)
	movq -6360(%rbp), %rax
	movq -2048(%rbp, %rax), %rcx
	movq %rcx, -6368(%rbp)
	movq -6168(%rbp), %rcx
	movq $16, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6376(%rbp)
	movq -6376(%rbp), %rcx
	movq -6160(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6384(%rbp)
	movq -6384(%rbp), %rcx
	movq $8, %rdx
	imulq %rcx, %rdx
	movq %rdx, -6392(%rbp)
	movq -6392(%rbp), %rax
	movq -4096(%rbp, %rax), %rcx
	movq %rcx, -6400(%rbp)
	movq -6368(%rbp), %rcx
	movq -6400(%rbp), %rdx
	imulq %rcx, %rdx
	movq %rdx, -6408(%rbp)
	movq -6336(%rbp), %rcx
	movq -6408(%rbp), %rdx
	addq %rdx, %rcx
	movq %rcx, -6416(%rbp)
	movq -6304(%rbp), %rax
	movq -6416(%rbp), %rcx
	movq %rcx, -6144(%rbp, %rax)
L21:
	movq -6168(%rbp), %rcx
	movq $1, %rdx
	addq %rdx, %rcx
	movq %rcx, -6168(%rbp)
	jmp L18
L19:
	movq -6160(%rbp), %rcx
	movq $1, %rdx
	addq %rdx, %rcx
	movq %rcx, -6160(%rbp)
	jmp L15
L16:
	movq -6152(%rbp), %rcx
	movq $1, %rdx
	addq %rdx, %rcx
	movq %rcx, -6152(%rbp)
	jmp L12
L13:
	movq $0, %rax
	jmp L2
L2:
	addq $6416, %rsp
	popq %rbp
	ret

