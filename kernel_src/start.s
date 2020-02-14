// We declare the 'kernel_main' label as being external to this file.
// That's because it's the name of the main C function in 'kernel.c'.
.extern kernel_main
 
// We declare the 'start' label as global (accessible from outside this file), since the linker will need to know where it is.
// In a bit, we'll actually take a look at the code that defines this label.
.global start

.global handle_int_80
.global start_entrypoint
 
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

.section .data
	.align 1 // byte backed data here
	null_gdt: .long 0
	          .long 0 // required
	code_gdt: .short 0xFFFF // limit 0:15
	          .short 0      // base 0:15
	          .byte 0       // base 16:23
	          .byte 0x9A    // Present, Ring 0, Normal, Executable, Non Conforming, Readable, Not accessed
	          .byte 0xCF    // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
	          .byte 0       // base 24:31
	data_gdt: .short 0xFFFF // limit 0:15
	          .short 0      // base 0:15
	          .byte 0       // base 16:23
	          .byte 0x92    // Present, Ring 0, Normal, Data, Grows up, Writable, Not accessed
	          .byte 0xCF    // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
	          .byte 0       // base 24:31
//	task_gdt: .long 0
//	          .long 0 // probably want something here eventually
	gdtr:     .short (gdtr - null_gdt - 1) // size (minus one)
	          .long null_gdt // offset

 
// This section contains data initialised to zeroes when the kernel is loaded
.section .bss
	// Our C code will need a stack to run. Here, we allocate 4096 bytes (or 4 Kilobytes) for our stack.
	// We can expand this later if we want a larger stack. For now, it will be perfectly adequate.
	.align 16
	stack_bottom:
		.skip 4096 // Reserve a 4096-byte (4K) stack
	stack_top:
	mb_header: .long
 
// This section contains our actual assembly code to be run when our kernel loads
.section .text
	// Here is the 'start' label we mentioned before. This is the first code that gets run in our kernel.
	start:
		// First thing's first: we want to set up an environment that's ready to run C code.
		// C is very relaxed in its requirements: All we need to do is to set up the stack.
		// Please note that on x86, the stack grows DOWNWARD. This is why we start at the top.
		mov $stack_top, %esp // Set the stack pointer to the top of the stack

		pushl %ebx // push multiboot header
		pushl %eax // push multiboot magic

		// setup the Global Descriptor Table with static values
		lgdtl gdtr
		jmpl $0x08, $reload_CS

		reload_CS:
		mov $0x10, %ax
		mov %ax, %ds
		mov %ax, %es
		mov %ax, %fs
		mov %ax, %gs
		mov %ax, %ss
		// GDT loaded!
 
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
		pushl %eax // push syscall number
		mov %esp, %eax
		addl $20, %eax // move 1 (caller IP) + 3 (interrupt) + 1 (our pushes)32-bit ints up the stack to the caller pushed values
		pushl %eax 
		call handle_int_80_impl // call into C now
		
		test %eax, %eax
		mov 16(%esp), %eax
		je error
		andl $0xFFFFFFFE, %eax
		jmp done
		
		error:
		orl $1, %eax
		jmp done

		done:
		mov %eax, 16(%esp)
		addl $4, %esp // skip frame pointer
		popl %eax
		iret

	// handle call from C, adjust the stack a bit, and jump to the
	// passed entrypoint
	start_entrypoint:
		addl $4, %esp // who needs a return address?
		popl %eax // get the destination address from the stack
		jmp *%eax
