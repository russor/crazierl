// We declare the 'kernel_main' label as being external to this file.
// That's because it's the name of the main C function in 'kernel.c'.
.extern kernel_main
 
// We declare the 'start' label as global (accessible from outside this file), since the linker will need to know where it is.
// In a bit, we'll actually take a look at the code that defines this label.
.global _start
.global ap_trampoline
.global ap_trampoline2

.global handle_int_80
.global start_entrypoint
.global gen_int
.global gen_error
.global setup_new_stack
.global setup_new_idle
.global switch_thread_impl
.global switch_ap_thread
.global GDT
.global stack_top
.global LOW_PAGE
.global thr_new_new_thread
.global idle

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

		mov (GDT), %ax
		mov %ax, tramp_gdtr
		mov $GDT, %eax
		mov %eax, 2 + GDT
		mov %eax, 2 + tramp_gdtr
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
		pushl %ecx
		pushl %edx
		pushl %esi
		pushl %edi
		pushl %ds
		pushl %es
		pushl %fs

		mov %gs, %dx
		andl $-16, %edx
		mov %dx, %gs
		lea 0x4(%ebp), %ecx

		pushl %ecx // interrupt frame
		pushl %eax // push syscall number
		call handle_syscall // call into C now
	handle_int_80_leave:
		addl $8, %esp // skip pushed syscall and frame pointer
		mov %gs, %dx
		orl $0xB, %edx
		mov %dx, %gs
		popl %fs
		popl %es
		popl %ds
		popl %edi
		popl %esi
		popl %edx
		popl %ecx
		popl %ebp
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

	.align 8
	gen_int:
		// repeat for possible vectors
		// skip 00-1F
		// 2x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 3x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 4x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 5x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 6x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 7x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 8x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// 9x
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// Ax
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// Bx
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// Cx
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// Dx
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// Ex
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler

		// Fx
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler
	.align 8
		call int_handler


	int_handler:
		xchg %ebp, (%esp)
		push %eax
		mov %ebp, %eax
		mov %esp, %ebp
		addl $4, %ebp
		push %ecx
		push %edx

		sub $(gen_int), %eax
		shr $3, %eax
		push %eax

		mov %gs, %dx            // set GS to kernel segment
		andl $-16, %edx
		mov %dx, %gs

		call handle_irq

		testl $0x3, 0x18(%esp)  // set GS to user segment, if pushed CS is user segment
		je gen_int_done
		mov %gs, %dx
		orl $0xB, %edx
		mov %dx, %gs

	gen_int_done:
		pop %eax
		pop %edx
		pop %ecx
		pop %eax
		pop %ebp
		iret

	.align 8
	gen_error:
		// repeat for possible vectors
		// 0x
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler

		// 1x
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler
	.align 8
		call err_handler

	err_handler:
		xchg %ebp, (%esp)
		push %eax
		mov %ebp, %eax
		mov %esp, %ebp
		addl $4, %ebp
		push %ecx
		push %edx

		lea 0x8(%ebp), %ecx
		pushl %ecx	// interrupt frame address
		pushl 0x4(%ebp) // error_code
		sub $(gen_error), %eax
		shr $3, %eax
		pushl %eax		// interrupt vector

		mov %gs, %dx            // set GS to kernel segment
		andl $-16, %edx
		mov %dx, %gs

		call handle_error

		testl $0x3, 0x24(%esp)  // set GS to user segment, if pushed CS is user segment
		je gen_error_done
		mov %gs, %dx
		orl $0xB, %edx
		mov %dx, %gs

	gen_error_done:
		addl $12, %esp
		pop %edx
		pop %ecx
		pop %eax
		pop %ebp
		iret

	setup_new_stack:
		push %ebp
		mov %esp, %ebp
		mov %esp, %ecx // current thread stack pointer
		mov 0xC(%ebp), %edx // current thread stack top
		mov 0x8(%ebp), %esp // temporarily use new thread stack top

		mov $5, %eax
	copy_iframe:
		subl $4, %edx
		pushl (%edx)
		dec %eax
		jne copy_iframe

		push $0 // %ebp
		subl $4, %edx
		mov %esp, %ebp

		mov $7, %eax
	copy_registers:
		subl $4, %edx
		pushl (%edx)
		dec %eax
		jne copy_registers

		subl $8, %esp
		pushl $handle_int_80_leave

		pushl $thr_new_new_thread
		push %ebp

		push %ebx
		push %ecx
		push %edx
		push %esi
		push %edi
		push $0 // return 0 for child
		mov %esp, %eax
		mov %ecx, %esp // return to current thread stack
	setup_new_stack_done:
		pop %ebp
		ret

	setup_new_idle:
		push %ebp
		mov %esp, %ebp
		mov %esp, %ecx // current stack pointer
		mov 0x8(%ebp), %eax // idle thread stack pointer
		mov %eax, %esp
		mov %esp, %ebp
		push $idle
		push %ebp
		mov %esp, %ebp
		push $setup_new_stack_done
		push %ebp
		push $0
		push $0
		push $0
		push $0
		push $0
		push $0
		mov %esp, %eax
		mov %ecx, %esp // return to current thread
		pop %ebp
		ret

	switch_thread_impl:
		push %ebp
		mov %esp, %ebp

		push %ebx
		push %ecx
		push %edx
		push %esi
		push %edi
		push $0 // return from switch_thread_impl, can be modified
		mov 0x8(%ebp), %eax
		mov %esp, (%eax) // copy stack to old_thread stack pointer
		mov 0xC(%ebp), %esp // copy new_thread stack pointer to stack
	switch_thread_done:
		pop %eax
		pop %edi
		pop %esi
		pop %edx
		pop %ecx
		pop %ebx

		pop %ebp
		ret

	switch_ap_thread:
		mov %eax, %esp
		jmp switch_thread_done

	.code16 // APs start in real mode
	ap_trampoline:
		cli
		mov $(tramp_gdtr - ap_trampoline), %eax
		lgdtl %cs:(%eax)
		mov %cr0, %eax
		or $1, %al
		mov %eax, %cr0 // enable protected mode
		jmpl $0x18, $ap_trampoline2 // bounce to the second part of the trampoline
	tramp_gdtr: .byte 0,0,0,0,0,0

	.code32
	ap_trampoline2:
		mov $0x28, %ax
		mov %ax, %ds
		mov %ax, %es
		mov %ax, %fs
		mov %ax, %ss

		mov LOW_PAGE, %eax
		addl $4096, %eax
		mov %eax, %esp
		call start_ap
