	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp

	movq	$32, %r13
	movq	$10, %r12
	addq	%r13, %r12
	movq	%r12, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$32, %rsp
	popq	%rbp
	retq
