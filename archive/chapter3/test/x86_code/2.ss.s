	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp

	movq	$-154, %r12
	movq	%r12, %r14
	addq	$101, %r14
	callq	read_int
	movq	%rax, %r12
	negq	%r12
	movq	%r12, %r13
	callq	read_int
	movq	%rax, %r12
	addq	%r13, %r12
	movq	%r14, %r13
	addq	%r12, %r13
	movq	%r13, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$64, %rsp
	popq	%rbp
	retq
