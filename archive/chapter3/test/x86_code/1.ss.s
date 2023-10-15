	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$128, %rsp

	callq	read_int
	movq	%rax, %r12
	negq	%r12
	movq	%r12, %r13
	callq	read_int
	movq	%rax, %r12
	negq	%r12
	addq	%r12, %r13
	negq	%r13
	movq	$235, %r12
	negq	%r12
	addq	$-35, %r12
	addq	%r12, %r13
	callq	read_int
	movq	%rax, %r12
	negq	%r12
	movq	%r12, %r14
	callq	read_int
	movq	%rax, %r12
	addq	$246, %r12
	addq	%r12, %r14
	movq	%r13, %r12
	addq	%r14, %r12
	movq	%r12, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$128, %rsp
	popq	%rbp
	retq
