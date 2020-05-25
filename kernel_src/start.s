// We declare the 'kernel_main' label as being external to this file.
// That's because it's the name of the main C function in 'kernel.c'.
.extern kernel_main
 
// We declare the 'start' label as global (accessible from outside this file), since the linker will need to know where it is.
// In a bit, we'll actually take a look at the code that defines this label.
.global _start

.global handle_int_80
.global start_entrypoint
.global unknown_int
.global ioapic_int
.global setup_new_stack
.global switch_thread_impl
.global GDT
.global stack_top

// Our bootloader, GRUB, needs to know some basic information about our kernel before it can boot it.
// We give GRUB this information using a standard known as 'Multiboot'.
// To define a valid 'Multiboot header' that will be recognised by GRUB, we need to hard code some
// constants into the executable. The following code calculates those constants.
.set MB_MAGIC, 0x1BADB002          // This is a 'magic' constant that GRUB will use to detect our kernel's location.
.set MB_FLAGS, (1 << 0) | (1 << 1) // This tells GRUB to 1: load modules on page boundaries and 2: provide a memory map (this is useful later in development)
// Finally, we calculate a checksum that includes all the previous values
.set MB_CHECKSUM, (0 - (MB_MAGIC + MB_FLAGS))
 
// We now start the section of the executable that will contain our Multiboot header
.section .multiboot
	.align 4 // Make sure the following data is aligned on a multiple of 4 bytes
	// Use the previously calculated constants in executable code
	.long MB_MAGIC
	.long MB_FLAGS
	// Use the checksum we calculated earlier
	.long MB_CHECKSUM

// This section contains data initialised to zeroes when the kernel is loaded
.global stack_top

.section .bss
	// Our C code will need a stack to run.
	.align 16
	stack_bottom:
		.skip 4096 // Reserve a stack
	stack_top:
 
// This section contains our actual assembly code to be run when our kernel loads
.section .text
	// Here is the 'start' label we mentioned before. This is the first code that gets run in our kernel.
	_start:
		// First thing's first: we want to set up an environment that's ready to run C code.
		// C is very relaxed in its requirements: All we need to do is to set up the stack.
		// Please note that on x86, the stack grows DOWNWARD. This is why we start at the top.
		mov $stack_top, %esp // Set the stack pointer to the top of the stack

		pushl %ebx // push multiboot header
		pushl %eax // push multiboot magic

		mov $GDT, %eax
		mov %eax, 2 + GDT
		// setup the Global Descriptor Table with static values
		lgdtl (%eax)
		jmpl $0x18, $reload_CS

		reload_CS:
		mov $0x28, %ax
		mov %ax, %ds
		mov %ax, %es
		mov %ax, %fs
		mov %ax, %ss

		// Now we have a C-worthy (haha!) environment ready to run the rest of our kernel.
		// At this point, we can call our main C function.
		call kernel_main
 
		// If, by some mysterious circumstances, the kernel's C code ever returns, all we want to do is to hang the CPU
		hang:
			cli      // Disable CPU interrupts
			hlt      // Halt the CPU
			jmp hang // If that didn't work, loop around and try again.
	// handle syscall interrupts
	handle_int_80:
		push   %ebp
		mov    %esp,%ebp
		pushl %ecx // save
		pushl %edx
		mov %gs, %dx
		andl $-16, %edx
		mov %dx, %gs
		mov %esp, %ecx
		addl $16, %ecx
		pushl %ecx
		pushl %eax // push syscall number
		call handle_syscall // call into C now
	handle_int_80_leave:
		addl $8, %esp // skip pushed syscall and frame pointer
		mov %gs, %dx
		orl $0xB, %edx
		popl %edx
		popl %ecx
		pop %ebp
		iret

	// handle call from C, adjust the stack a bit, and jump to the
	// passed entrypoint
	start_entrypoint:
		popl %eax // who needs a return address?
		popl %ebx // new stack top
		popl %ecx // entrypoint
		popl %eax // gs segment

		mov %ax, %gs
		mov $0x23, %eax
		mov %ax, %ds
		mov %ax, %es
		mov %ax, %fs

		pushl %eax
		pushl %ebx

		pushf
		popl %eax
		orl $0x3200, %eax // enable interrupts, and allow ring 3 to do IO
		pushl %eax

		pushl $0x13
		pushl %ecx
		iret

	unknown_int:
		// repeat 256 times
		// 0x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 1x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 2x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 3x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 4x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 5x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 6x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 7x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 8x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// 9x
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// Ax
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// Bx
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// Cx
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// Dx
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// Ex
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

		// Fx
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler
		call call_handler

	call_handler:
		xchg %eax, (%esp)
		push %ecx
		push %edx

		sub $(unknown_int + 5), %eax
		xor %edx, %edx
		mov $5, %ecx
		idivw %cx
		push %eax
		mov %esp, %eax
		addl $16, %eax
		push %eax
		call handle_unknown_irq
		addl $8, %esp
		pop %edx
		pop %ecx
		pop %eax
		iret

	ioapic_int:
		// repeat 8 times
		call ioapic_call_handler
		call ioapic_call_handler
		call ioapic_call_handler
		call ioapic_call_handler

		call ioapic_call_handler
		call ioapic_call_handler
		call ioapic_call_handler
		call ioapic_call_handler

	ioapic_call_handler:
		xchg %eax, (%esp)
		push %ecx
		push %edx

		sub $(ioapic_int + 5), %eax
		xor %edx, %edx
		mov $5, %ecx
		idivw %cx
		push %eax
		mov %esp, %eax
		addl $16, %eax
		push %eax
		call handle_ioapic_irq
		addl $8, %esp
		pop %edx
		pop %ecx
		pop %eax
		iret

	setup_new_stack:
		push %ebp
		mov %esp, %ebp
		mov %esp, %ecx // current thread stack pointer
		mov 0x8(%ebp), %eax // new thread stack pointer
		mov %eax, %esp // temporarily use new stack

		push 0x4(%ebp) // push return to thr_new
		push %ebp
		mov %esp, %ebp
		push $setup_new_stack_done // return address for switch_thread_impl
		push %ebp
		mov %esp, %ebp // will need ebp to save it
		push %ebx
		push %ecx
		push %edx
		push %esi
		push %edi
		push %ebp
		push %gs
		push $0 // return 0 for child
		mov %esp, %eax
		mov %ecx, %esp // return to current thread stack
	setup_new_stack_done:
		pop %ebp
		ret


	switch_thread_impl:
		push %ebp
		mov %esp, %ebp

		push %ebx
		push %ecx // maybe not strictly required
		push %edx // maybe not strictly required
		push %esi
		push %edi
		push %ebp
		push %gs // needed in case of timer interrupt
		push $0 // return from switch_thread_impl, can be modified
		mov 0x8(%ebp), %eax
		mov %esp, (%eax) // copy stack to old_thread stack pointer
		mov 0xC(%ebp), %eax
		mov %eax, %esp // copy new_thread stack pointer to stack
	switch_thread_done:
		pop %eax
		pop %gs
		pop %ebp
		pop %edi
		pop %esi
		pop %edx
		pop %ecx
		pop %ebx

		pop %ebp
		ret
