	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp

	callq	read_int
	movq	%rax, %r13
	callq	read_int
	movq	%rax, %r12
	negq	%r12
	addq	$148, %r12
	addq	%r12, %r13
	movq	%r13, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$48, %rsp
	popq	%rbp
	retq
