	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp

	callq	read_int
	movq	%rax, %r12
	movq	$-143, %r14
	addq	%r12, %r14
	movq	$54, %r13
	movq	$153, %r12
	addq	%r13, %r12
	movq	%r14, %r13
	addq	%r12, %r13
	movq	%r13, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$48, %rsp
	popq	%rbp
	retq
