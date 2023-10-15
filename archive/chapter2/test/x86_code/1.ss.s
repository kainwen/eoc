	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp

	callq	read_int
	movq	%rax, -56(%rbp)
	callq	read_int
	movq	%rax, -40(%rbp)
	callq	read_int
	movq	%rax, -16(%rbp)
	negq	-16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	callq	read_int
	movq	%rax, -8(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	-8(%rbp), %rax
	addq	%rax, -32(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -48(%rbp)
	movq	-56(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-48(%rbp), %rax
	addq	%rax, -64(%rbp)
	movq	$-24, -72(%rbp)
	movq	-64(%rbp), %rax
	addq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$80, %rsp
	popq	%rbp
	retq
