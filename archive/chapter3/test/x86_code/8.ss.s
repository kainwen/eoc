	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp

	movq	$1, %r12
	movq	$46, %r14
	movq	%r12, %r13
	addq	$7, %r13
	movq	$4, %r12
	addq	%r13, %r12
	addq	%r14, %r13
	negq	%r12
	addq	%r12, %r13
	movq	%r13, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$64, %rsp
	popq	%rbp
	retq
