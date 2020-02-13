// GCC provides these header files automatically
// They give us access to useful things like fixed-width types
#include <stddef.h>
#include <stdint.h>

extern const char *syscallnames[];
extern void * handle_int_80;
 
#define PORT 0x3f8   /* COM1 */

// First, let's do some basic checks to make sure we are using our x86-elf cross-compiler correctly
#if defined(__linux__)
	#error "This code must be compiled with a cross-compiler"
#elif !defined(__i386__)
	#error "This code must be compiled with an x86-elf compiler"
#endif

#include <stdio.h>
#include "/usr/src/stand/i386/libi386/multiboot.h"
typedef uint32_t u_int32_t;

#include <sys/elf32.h>

#include <errno.h>

uint8_t WANT_NMI = 0;
 
// This is the x86's VGA textmode buffer. To display text, we write data to this memory location
volatile uint16_t* vga_buffer = (uint16_t*)0xB8000;
// By default, the VGA textmode buffer has a size of 80x25 characters
const int VGA_COLS = 80;
const int VGA_ROWS = 25;
 
// We start displaying text in the top-left of the screen (column = 0, row = 0)
int term_col = 0;
int term_row = 0;
uint8_t term_color = 0x0F; // Black background, White foreground

struct IDTDescr {
   uint16_t offset_1; // offset bits 0..15
   uint16_t selector; // a code segment selector in GDT or LDT
   uint8_t zero;      // unused, set to 0
   uint8_t type_attr; // type and attributes, see below
   uint16_t offset_2; // offset bits 16..31
} __attribute((packed));

struct IDTRecord {
   uint16_t size;
   uint32_t offset;
} __attribute((packed));

struct IDTDescr IDT[256]; // need enough to handle General Protection Fault
struct IDTRecord IDTR;

volatile uint32_t TIMER_COUNT = 0;

uint8_t last_time[9];

static inline void outb(uint16_t port, uint8_t val)
{
	asm volatile ( "outb %0, %1" : : "a"(val), "Nd"(port) );
	/* There's an outb %al, $imm8  encoding, for compile-time constant port numbers that fit in 8b.  (N constraint).
	 * Wider immediate constants would be truncated at assemble-time (e.g. "i" constraint).
	 * The  outb  %al, %dx  encoding is the only option for all other cases.
	 * %1 expands to %dx because  port  is a uint16_t.  %w1 could be used if we had the port number a wider C type */
}

static inline uint8_t inb(uint16_t port)
{
	uint8_t ret;
	asm volatile ( "inb %1, %0"
	               : "=a"(ret)
	               : "Nd"(port) );
	return ret;
}
 
// This function initiates the terminal by clearing it
void term_init()
{
	// Clear the textmode buffer
	for (int col = 0; col < VGA_COLS; col ++)
	{
		for (int row = 0; row < VGA_ROWS; row ++)
		{
			// The VGA textmode buffer has size (VGA_COLS * VGA_ROWS).
			// Given this, we find an index into the buffer for our character
			const size_t index = (VGA_COLS * row) + col;
			// Entries in the VGA buffer take the binary form BBBBFFFFCCCCCCCC, where:
			// - B is the background color
			// - F is the foreground color
			// - C is the ASCII character
			vga_buffer[index] = ((uint16_t)term_color << 8) | ' '; // Set the character to blank (a space character)
		}
	}
	// init com port
	outb(PORT + 1, 0x00);    // Disable all interrupts
	outb(PORT + 3, 0x80);    // Enable DLAB (set baud rate divisor)
	outb(PORT + 0, 0x03);    // Set divisor to 3 (lo byte) 38400 baud
	outb(PORT + 1, 0x00);    //                  (hi byte)
	outb(PORT + 3, 0x03);    // 8 bits, no parity, one stop bit
	outb(PORT + 2, 0xC7);    // Enable FIFO, clear them, with 14-byte threshold
	outb(PORT + 4, 0x0B);    // IRQs enabled, RTS/DSR set
}

int is_transmit_empty() {
   return inb(PORT + 5) & 0x20;
}

void write_serial(char a) {
   while (is_transmit_empty() == 0);
 
   outb(PORT,a);
}

// This function places a single character onto the screen
void _putchar(char c)
{
	// Remember - we don't want to display ALL characters!
	switch (c)
	{
	case '\n': // Newline characters should return the column to 0, and increment the row
		{
			term_col = 0;
			term_row ++;
			break;
		}
 
	default: // Normal characters just get displayed and then increment the column
		{
			const size_t index = (VGA_COLS * term_row) + term_col; // Like before, calculate the buffer index
			vga_buffer[index] = ((uint16_t)term_color << 8) | c;
			term_col ++;
			break;
		}
	}
 
	// What happens if we get past the last column? We need to reset the column to 0, and increment the row to get to a new line
	if (term_col >= VGA_COLS)
	{
		term_col = 0;
		term_row ++;
	}
 
	// What happens if we get past the last row? We need to reset both column and row to 0 in order to loop back to the top of the screen
	if (term_row >= VGA_ROWS)
	{
		term_col = 0;
		term_row = 0;
	}
	if (term_col == 0) {
		size_t index = (VGA_COLS * term_row); // Like before, calculate the buffer index
		for (int i = 0; i < 80; ++i) {
			vga_buffer[index +i] = ((uint16_t)term_color << 8) | ' ';
		}
	}
	write_serial(c);
}
 
// This function prints an entire string onto the screen
void term_print(const char* str)
{
	for (size_t i = 0; str[i] != '\0'; i ++) // Keep placing characters until we hit the null-terminating character ('\0')
		_putchar(str[i]);
}
 
uint8_t read_cmos(uint8_t reg)
{
	outb(0x70, WANT_NMI | reg);
	return inb(0x71);
}

void memcpy(uint8_t *dst, uint8_t *src, unsigned int count)
{
	while (count) {
		--count;
		*dst = *src;
		++dst;
		++src;
	}
}

uint8_t memcmp(uint8_t *a, uint8_t *b, unsigned int count)
{
	while (1) {
		if (!count) { return 0; }
		if (*a < *b) { return -1; }
		if (*b > *a) { return 1; }
		--count;
		++a;
		++b;
	}
}

void get_time(uint8_t timeA[])
{
	uint8_t timeB[9];
	do {
		memcpy(timeB, timeA, sizeof(timeB));
		while (((timeA[0] = read_cmos(0x0A)) & 0x80) != 0) { } // Status Register A
		timeA[1] = read_cmos(0x00); // Seconds
		timeA[2] = read_cmos(0x02); // Minutes
		timeA[3] = read_cmos(0x04); // Hours
		timeA[4] = read_cmos(0x07); // Day of Month (ignore weekday)
		timeA[5] = read_cmos(0x08); // Month
		timeA[6] = read_cmos(0x09); // Year
		timeA[7] = read_cmos(0x32); // Century (maybe)
		timeA[8] = read_cmos(0x0B); // Status Register B
	} while (memcmp(timeA, timeB, sizeof(timeB)) != 0);

	uint8_t tens, ones;
	if ((timeA[8] & 0x04) == 0) { // BCD mode
		for (int i = 1; i <= 7; ++i) {
			timeA[i] = ((timeA[i] >> 4) * 10) + (timeA[i] & 0x0F);
		}
	}

	if ((timeA[8] & 0x02) == 0) { // 12 hour mode
		if (timeA[3] & 0x80) {
			timeA[3] = 12 + (timeA[3] & 0x7F);
		} else if (timeA[3] == 12) {
			timeA[3] = 0;
		}
	}
}
	
void print_time(uint8_t time[9]) {
	printf("%02d%02d-%02d-%02d %02d:%02d:%02d\n",
		time[7], time[6], time[5], time[4], time[3], time[2], time[1]);
}

struct interrupt_frame
{
    uint32_t ip;
    uint32_t cs;
    uint32_t flags;
    uint32_t sp;
    uint32_t ss;
};

__attribute__ ((interrupt))
void handle_timer(struct interrupt_frame *frame)
{
	uint8_t new_time[9];
	get_time(new_time);
	//if (1) { 
	if (memcmp(last_time, new_time, sizeof(last_time)) != 0) {
		memcpy(last_time, new_time, sizeof(new_time));
		printf("Got RTC interrupt IP: %08x at ", frame->ip);
		print_time(last_time);
	}
	// clear RTC flag
	//outb(0x70, 0x0C);	// select register C
	//inb(0x71);		// just throw away contents
	++TIMER_COUNT;
}

uint32_t handle_int_80_impl(uint32_t *frame, uint32_t call)
{	
	switch(call) {
		case 58:
			printf("readlink (%s, %08x, %d)\n", (char *)frame[0], frame[1], frame[2]);
			call = ENOENT;
			return 0;
	}
				
	if (call <= 567) {
		printf("Got syscall %d (%s)@%08x at ", call, syscallnames[call], &call);
	} else {
		printf("got unknown syscall %d at ", call);
	}
	print_time(last_time);
	while (1) {
	}
}

__attribute__ ((interrupt))
void handle_gp(struct interrupt_frame *frame, uint32_t error_code)
{
	if (frame) {
		printf("Got #GP (%u) IP: %08x at ", error_code, frame->ip);
	} else {
		printf("Got #GP, no stack frame\n");
	}
	print_time(last_time);
	while (1) { } // loop forever
}

__attribute__ ((interrupt))
void handle_ud(struct interrupt_frame *frame)
{
	printf("Got #UD IP: %08x at ", frame->ip);
	print_time(last_time);
	while (1) { } // loop forever
}
void interrupt_setup()
{
	// remap primary PIC so that the interrupts don't conflict with Intel exceptions
	
	uint8_t mask = inb(0x21); // read current mask from PIC
	outb(0x20, 0x11); // request initialization
	outb(0x80, 0); // wait cycle
	outb(0x21, 0x20); // offset interrupts by 0x20
	outb(0x80, 0); // wait
	outb(0x21, 0x4); // indicate slave PIC on IRQ 2
	outb(0x80, 0); //wait
	outb(0x21, 0x01); // set to 8086 mode
	outb(0x80, 0); //wait
	outb(0x21, mask); // reset interrupt mask

	// ensure IDT entries are not present
	for (int i = 0; i < (sizeof(IDT) / sizeof(IDT[0])); ++i) {
		IDT[i].type_attr = 0;
	}
	IDT[0x06].offset_1 = ((uint32_t) &handle_ud) & 0xFFFF;
	IDT[0x06].selector = 0x08;
	IDT[0x06].zero = 0;
	IDT[0x06].type_attr = 0x8E;
	IDT[0x06].offset_2 = ((uint32_t) &handle_ud) >> 16;

	IDT[0x0D].offset_1 = ((uint32_t) &handle_gp) & 0xFFFF;
	IDT[0x0D].selector = 0x08;
	IDT[0x0D].zero = 0;
	IDT[0x0D].type_attr = 0x8E;
	IDT[0x0D].offset_2 = ((uint32_t) &handle_gp) >> 16;

	IDT[0x20].offset_1 = ((uint32_t) &handle_timer) & 0xFFFF;
	IDT[0x20].selector = 0x08;
	IDT[0x20].zero = 0;
	IDT[0x20].type_attr = 0x8E;
	IDT[0x20].offset_2 = ((uint32_t) &handle_timer) >> 16;

	IDT[0x80].offset_1 = ((uint32_t) &handle_int_80) & 0xFFFF;
	IDT[0x80].selector = 0x08;
	IDT[0x80].zero = 0;
	IDT[0x80].type_attr = 0x8E;
	IDT[0x80].offset_2 = ((uint32_t) &handle_int_80) >> 16;
	
	IDTR.size = sizeof(IDT) - 1;
	IDTR.offset = (uint32_t) &IDT;
	asm volatile ( "lidt %0" :: "m" (IDTR) );
	printf("loaded idtl of size 0x%04x\n", IDTR.size);
	asm volatile ( "sti" :: );
}

unsigned int min(unsigned int a, unsigned int b) {
	if (a < b) { return a; }
	return b;
}
void (*entrypoint)(void);

void load_module(multiboot_module_t mod) {
	Elf32_Ehdr * head = (void *)mod.mod_start;
	entrypoint = (void *) head->e_entry;
	printf ("elf entrypoint 0x%08x\n", head->e_entry);
	printf ("%d program headers of size %d\n", head->e_phnum, head->e_phentsize);
	
	Elf32_Phdr *phead = (void *)mod.mod_start + head->e_phoff;
	for (int i = 0; i < head->e_phnum; ++i) {
		printf( "  %d: type 0x%x, offset %08x, virt %08x, filesize 0x%08x, memsize 0x%08x\n",
			i, phead->p_type, phead->p_offset, phead->p_vaddr,
			phead->p_filesz, phead->p_memsz);
		++phead;
		if (phead->p_type == PT_LOAD) {
			unsigned int count = min(phead->p_filesz, phead->p_memsz);
			uint8_t *src = (void*) mod.mod_start + phead->p_offset;
			uint8_t *dst = (void*) phead->p_vaddr;
			printf("copying %d bytes from %08x to %08x\n", count, src, dst);
			memcpy(dst, src, count);
		}
	}
}

void enable_sse() {
/*	mov %cr0, %eax
	and 0xFFFFFFFB, %ax		//clear coprocessor emulation CR0.EM
	or 0x2, %eax			//set coprocessor monitoring  CR0.MP
//	mov %eax, %cr0
	mov %cr4, %eax
	or 3 << 9, %eax		//set CR4.OSFXSR and CR4.OSXMMEXCPT at the same time
//	mov %eax, %cr4
*/
	uint32_t a,b,c,d;
	uint32_t code = 1;
	asm volatile("cpuid":"=a"(a),"=b"(b),"=c"(c),"=d"(d):"a"(code));
	printf("cpuid eax %08x, ebx %08x, ecx %08x, edx %08x\n", a, b, c, d);
	// https://wiki.osdev.org/SSE#Adding_support
	asm volatile("mov %%cr0, %0" : "=a" (a));
	printf("cr0: %08x\n", a);
	a&= 0xFFFFFFFB;
	a|= 2;
	printf("cr0: %08x\n", a);
	asm volatile("mov %0, %%cr0" :: "a"(a));
	asm volatile("mov %%cr4, %0" : "=a" (a));
	printf("cr4: %08x\n", a);
	a|= (3 << 9);
	printf("cr4: %08x\n", a);
	asm volatile("mov %0, %%cr4" :: "a"(a));
}

// This is our kernel's main function
void kernel_main(uint32_t mb_magic, multiboot_info_t *mb)
{
	// We're here! Let's initiate the terminal and display a message to show we got here.
 
	// Initiate terminal
	term_init();
 
	// Display some messages
	term_print("Hello, World!\n");
	term_print("Welcome to the kernel at ");
	get_time(last_time);
	print_time(last_time);

	printf("kernel main at %08x\n", kernel_main);
	printf("Multiboot magic: %08x (%s)\n", mb_magic, (char *) mb->boot_loader_name);
	printf("Multiboot info at %08x (%08x)\n", mb, &mb);
	printf("mem range: %08x-%08x\n", mb->mem_lower, mb->mem_upper);
	printf("modules: %d @ %08x\n", mb->mods_count, mb->mods_addr);
	
	multiboot_module_t *mods = (void *)mb->mods_addr;
	for (int mod = 0; mod < mb->mods_count; ++mod) {
		printf("Module %d (%s):\n 0x%08x-0x%08x\n", mod, mods[mod].cmdline, mods[mod].mod_start, mods[mod].mod_end);
		load_module(mods[mod]);
	}
	
	
	printf("memory map: %d @ %08x\n", mb->mmap_length, mb->mmap_addr);
      multiboot_memory_map_t *mmap;
      
      printf ("mmap_addr = 0x%x, mmap_length = 0x%x\n",
              (unsigned) mb->mmap_addr, (unsigned) mb->mmap_length);
      for (mmap = (multiboot_memory_map_t *) mb->mmap_addr;
           (unsigned long) mmap < mb->mmap_addr + mb->mmap_length;
           mmap = (multiboot_memory_map_t *) ((unsigned long) mmap
                                    + mmap->size + sizeof (mmap->size)))
        printf (" size = 0x%x, base_addr = 0x%08x,"
                " length = 0x%08x, type = 0x%x\n",
                (unsigned) mmap->size,
                //(unsigned) (mmap->addr >> 32),
                (unsigned) (mmap->addr & 0xffffffff),
                //(unsigned) (mmap->len >> 32),
                (unsigned) (mmap->len & 0xffffffff),
                (unsigned) mmap->type);	
	
	interrupt_setup();
	
	enable_sse();
	
	if (entrypoint) {
		printf ("jumping to %08x\n", entrypoint);
		entrypoint();
	}
	//get_time();
	while (1) {
		while (TIMER_COUNT) {
			outb(0x20,0x20);
			term_print(".");
			--TIMER_COUNT;
		}
	}
}

