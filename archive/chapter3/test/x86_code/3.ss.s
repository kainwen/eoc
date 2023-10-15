	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp

	movq	$-7, %r12
	addq	$65, %r12
	movq	$-52, %r13
	addq	%r12, %r13
	movq	%r13, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$32, %rsp
	popq	%rbp
	retq
