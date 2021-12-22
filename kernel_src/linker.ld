/* The bootloader will start execution at the symbol designated as the entry point. In this case, that's 'start' (defined in start.s) */
ENTRY(_start)
 
/* Tell the linker part of the compiler where the various sections of the kernel will be put in the final kernel executable. */
SECTIONS
{
	/* Begin putting sections at 1 Megabyte (1M), a good place for kernels to be loaded at by the bootloader. */
	/* This is because memory below 1 Megabyte is reserved for other x86-related things, so we can't use it */
 
	/* We align all sections in the executable at multiples of 4 Kilobytes (4K). This will become useful later in development when we add paging */
 
	/* First put the multiboot header, as it's required to be near the start of the executable otherwise the bootloader won't find it */
	/* The Multiboot header is Read-Only data, so we can put it in a '.rodata' section. */
	. = 1M;

	.rodata : AT(1M)
	{
		PROVIDE (__executable_start = .);
		*(.multiboot)
		*(.rodata)
	}

	/* Executable code */
	.text : ALIGN(4K)
	{
		. = ALIGN(4K);
		*(.text)
		PROVIDE (__etext = .);

	/* Read-write data (initialized) */
	.data : ALIGN(4K)
	{
		. = ALIGN(4K);
		PROVIDE (__data_start = .);
		*(.data)
	}

	.tdata : ALIGN(4K)
	{
		. = ALIGN(4K);
		PROVIDE (__tdata_start = .);
		*(.tdata)
		*(.tbss)
	}

	/* Read-write data (uninitialized) and stack */
	.bss : ALIGN(4K)
	{
		. = ALIGN(4K);
		PROVIDE (__tdata_end = .);
		PROVIDE (__locks_start = .);
		*(locks)
		PROVIDE (__locks_end = .);
		*(COMMON)
		*(.bss)
		PROVIDE (__edata = .);
	}
}
