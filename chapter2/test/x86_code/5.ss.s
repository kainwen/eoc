	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp

	callq	read_int
	movq	%rax, -8(%rbp)
	movq	$64, -16(%rbp)
	movq	-8(%rbp), %rax
	addq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$16, %rsp
	popq	%rbp
	retq
