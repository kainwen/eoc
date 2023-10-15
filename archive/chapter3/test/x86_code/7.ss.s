	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp

	movq	$4, %r12
	addq	$1, %r12
	addq	$2, %r12
	movq	%r12, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$32, %rsp
	popq	%rbp
	retq
