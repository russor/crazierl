#include <sys/types.h>
#include <printf.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

extern const char *syscallnames[];
extern void * handle_int_80;
extern void * unknown_int;
extern void start_entrypoint(); 

char **environ = {NULL};
char *__progname = "crazierlkernel";

#define BOGFD_CLOSED 0
#define BOGFD_TERMIN 1
#define BOGFD_TERMOUT 2
#define BOGFD_PIPE 3
#define BOGFD_FILE 4


struct BogusFD {
	int type;
	void* data;
	void* offset;
};

#define BOGFD_MAX 1024
struct BogusFD FDS[BOGFD_MAX];
size_t next_fd;


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
 
#include <x86/elf.h>
//#include <sys/elf32.h>

#include <errno.h>
#include <sys/sysctl.h>
#include <vm/vm_param.h>
#include <sys/syscall.h>
#include <time.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <term.h>
#include <sys/resource.h>
#include <machine/sysarch.h>

#include "files.h"

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

struct GDTDescr {
   uint16_t limit_1;
   uint16_t base_1;
   uint8_t base_2;
   uint8_t access;
   uint8_t limit_2;
   uint8_t base_3;
} __attribute((packed));

extern struct GDTDescr gs_base;

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

struct IDTDescr IDT[256];
struct IDTRecord IDTR;

volatile uint32_t TIMER_COUNT = 0;

uint8_t last_time[9];

uint32_t free_addr;
uint32_t max_addr;

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

unsigned int min(unsigned int a, unsigned int b) {
	if (a < b) { return a; }
	return b;
}

unsigned int max(unsigned int a, unsigned int b) {
	if (a > b) { return a; }
	return b;
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
	write_serial(c);
	// Remember - we don't want to display ALL characters!
	switch (c)
	{
	case '\r': break;
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
		// clear the line if we're starting a new line
		size_t index = (VGA_COLS * term_row);
		for (int i = 0; i < 80; ++i) {
			vga_buffer[index +i] = ((uint16_t)term_color << 8) | ' ';
		}
	}
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

uint32_t unix_time() {
	int years = (last_time[7] * 100 + last_time[6]) - 1970;
	int days = years * 365 + (years >> 2) + last_time[4];
	if (last_time[5] == 1 || (last_time[5] == 2 && last_time[4] <= 29)) {
		--days;
	}
	switch (last_time[5]) {
		case 12: days += 30;
		case 11: days += 31;
		case 10: days += 30;
		case  9: days += 31;
		case  8: days += 31;
		case  7: days += 30;
		case  6: days += 31;
		case  5: days += 30;
		case  4: days += 31;
		case  3: days += 28;
		case  2: days += 31;
	}
	return days * 86400 + last_time[3] * 3600 + last_time[2] * 60 + last_time[1];
}
	
void print_time(uint8_t time[9]) {
	printf("%02d%02d-%02d-%02d %02d:%02d:%02d (%d)\n",
		time[7], time[6], time[5], time[4], time[3], time[2], time[1], unix_time());
}

struct interrupt_frame
{
    uint32_t ip;
    uint32_t cs;
    uint32_t flags;
    uint32_t sp;
    uint32_t ss;
};

void handle_unknown_irq(struct interrupt_frame *frame, uint32_t irq)
{
	printf("Got unexpected interrupt 0x%02x IP: %08x at ", irq, frame->ip);
	print_time(last_time);
	while (1) { }
}

void handle_unknown_error(struct interrupt_frame *frame, uint32_t irq, uint32_t error_code)
{
	printf("Got unexpected error %d (%d) IP: %08x at ", irq, error_code, frame->ip);
	print_time(last_time);
	while (1) { }
}

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
	outb(0x20,0x20);
	// clear RTC flag
	//outb(0x70, 0x0C);	// select register C
	//inb(0x71);		// just throw away contents
	++TIMER_COUNT;
}

uint32_t handle_int_80_impl(uint32_t *frame, uint32_t call)
{	
	switch(call) {
		case SYS_read:
			printf("read (%d, %08x, %d)\n", frame[0], frame[1], frame[2]);
			if (frame[0] < BOGFD_MAX && FDS[frame[0]].type == BOGFD_FILE) {
				struct BogusFD *fd = &FDS[frame[0]];
				struct hardcoded_file * file = (struct hardcoded_file *)fd->data;
				call = min(frame[2], file->end - fd->offset);
				memcpy((void *)frame[1], fd->offset, call);
				fd->offset += call;
				return 1;
			}
			call = EBADF;
			return 0;
		case SYS_write:
			if (frame[0] < BOGFD_MAX && FDS[frame[0]].type == BOGFD_TERMOUT) {
				uint8_t *buffer = (uint8_t *)frame[1];
				call = frame[2];
				size_t nbyte = call;
				while (nbyte) {
					_putchar(*buffer);
					++buffer;
					--nbyte;
				}
				return 1;
			} else {
				printf("write (%d, %08x, %d)\n", frame[0], frame[1], frame[2]);
				call = EBADF;
				return 0;
			}
		case SYS_open:
			printf ("open (%s, %d)\n", frame[0], frame[1]);
			call = ENOENT;
			return 0;
		case SYS_close:
			if (FDS[frame[0]].type != BOGFD_CLOSED) { 
				printf("close (%d)\n", frame[0]);
			}
			FDS[frame[0]].type = BOGFD_CLOSED;
			if (frame[0] + 1 == next_fd) {
				--next_fd;
			}
			call = 0;
			return 1;
		case SYS_access:
			printf ("access (%s, %d)\n", frame[0], frame[1]);
			call = ENOENT;
			return 0;
		case SYS_ioctl:
			printf("ioctl (%d, %08x, ...)\n", frame[0], frame[1]);
			switch (frame[1]) {
				case TIOCGETA:
					if (frame[0] == 0) {
						struct termios *t = (struct termios *)frame[2];
						t->c_iflag = 11010;
						t->c_oflag = 3;
						t->c_cflag = 19200;
						t->c_lflag = 1483;
						//t->c_cc = "\004\377\377\177\027\025\022\b\003\034\032\031\021\023\026\017\001\000\024\377";
						t->c_ispeed = 38400;
						t->c_ospeed = 38400;
						call = 0;
						return 1;
					} else {
						printf("TIOCGETA\n");
					}
					break;
			}
			printf("parm_len %d, cmd %d, group %c\n", IOCPARM_LEN(frame[1]), frame[1] & 0xff, IOCGROUP(frame[1]));
			break;
		case SYS_readlink:
			printf("readlink (%s, %08x, %d)\n", (char *)frame[0], frame[1], frame[2]);
			call = ENOENT;
			return 0;
		case SYS_munmap:
			printf("munmap (%08x, %d)\n",
				frame[0], frame[1]);
			if (frame[0] + frame[1] == free_addr) {
				free_addr = frame[0];
				printf("free_addr -> %08x\n", free_addr);
			}
			call = 0;
			return 1;
		case SYS_madvise: //ignore
			call = 0;
			return 0;
		case SYS_fcntl:
			printf("fcntl (%d, %08x)\n", frame[0], frame[1]);
			call = 0;
			return 1;
		case SYS_socket:
			printf("socket(%d, %d, %d)\n", frame[0], frame[1], frame[2]);
			call = EACCES;
			return 0;
		case SYS_gettimeofday:
			//printf("gettimeofday (%08x, %08x)\n", frame[0], frame[1]);
			if (frame[0]) {
				((struct timeval *) frame[0])->tv_sec = unix_time();
				((struct timeval *) frame[0])->tv_usec = 0;
			}
			if (frame[1]) {
				((struct timezone *) frame[1])->tz_minuteswest = 0;
				((struct timezone *) frame[1])->tz_dsttime = 0;
			}
			call = 0;
			return 1;
		case SYS_sysarch: {
			switch (frame[0]) {
				case I386_SET_GSBASE: {
					uint32_t base = *((uint32_t *) frame[1]);
					gs_base.base_1 = base & 0xFFFF;
					base >>= 16;
					gs_base.base_2 = base & 0xFF;
					base >>= 8;
					gs_base.base_3 = base & 0xFF;
					base = 0x18;
					asm volatile ( "movw %0, %%gs" :: "rm" (base));
					call = 0;
					return 1;
				}
				
				
				}
			}
		case SYS_getrlimit: {
			struct rlimit *rlp = (struct rlimit *) frame[1];
			switch (frame[0]) {
				case RLIMIT_STACK:
					rlp->rlim_cur = 1024 * 1024;
					rlp->rlim_max = 1024 * 1024;
					break;
				case RLIMIT_NOFILE:
					rlp->rlim_cur = BOGFD_MAX;
					rlp->rlim_max = BOGFD_MAX;
					break;
				default:
					rlp->rlim_cur = RLIM_INFINITY;
					rlp->rlim_max = RLIM_INFINITY;
			}
			call = 0;
			return 1;
			}
		case SYS___sysctl:
			if (frame[1] == 2) {
				uint32_t *buffer = (uint32_t *)frame[0];
				switch (buffer[0]) {
					case CTL_KERN: switch(buffer[1]) {
						case KERN_OSTYPE:
							strlcpy((char *)frame[2], "FreeBSD", frame[3]);
							call = 0;
							return 1;
						case KERN_OSRELEASE:
							strlcpy((char *)frame[2], "12.1-RELEASE-p1", frame[3]);
							call = 0;
							return 1;
						case KERN_VERSION:
							strlcpy((char *)frame[2], "FreeBSD 12.1-RELEASE-p1 GENERIC", frame[3]);
							call = 0;
							return 1;
						case KERN_HOSTNAME:
							strlcpy((char *)frame[2], "node0.crazierl.org", frame[3]);
							call = 0;
							return 1;
						case KERN_ARND:
							{
								uint8_t *r = (uint8_t *)frame[2];
								uint32_t count = *((uint32_t *)frame[3]);
								printf("random requested (%d)\n", count);
								while (count) {
									*r = rand() & 0xFF;
									--count;
								}
								call = 0;
								return 1;
							}
						case KERN_OSRELDATE:
							*(uint32_t *)frame[2] = 1201000; // pretend to be freebsd 12.1 for now
							*(uint32_t *)frame[3] = sizeof(uint32_t);
							call = 0;
							return 1;
						}
					case CTL_VM: switch (buffer[1]) {
						case VM_OVERCOMMIT:
							*(uint32_t *)frame[2] = 0;
							*(uint32_t *)frame[3] = sizeof(uint32_t);
							call = 0;
							return 1;
						}
					case CTL_HW: switch (buffer[1]) {
						case HW_MACHINE:
							strlcpy((char *)frame[2], "i386", frame[3]);
							call = 0;
							return 1;
						case HW_NCPU:
							printf("hw.ncpu\n");
							*(uint32_t *)frame[2] = 1;
							*(uint32_t *)frame[3] = sizeof(uint32_t);
							call = 0;
							return 1;
						case HW_PAGESIZE:
							printf("hw.pagesize\n");
							*(uint32_t *)frame[2] = 4096;
							*(uint32_t *)frame[3] = sizeof(uint32_t);
							call = 0;
							return 1;
						}
					case CTL_P1003_1B: switch (buffer[1]) {
						case CTL_P1003_1B_PAGESIZE:
							printf("posix.pagesize\n");
							*(uint32_t *)frame[2] = 4096;
							*(uint32_t *)frame[3] = sizeof(uint32_t);
							call = 0;
							return 1;
						}
				}
			}
			printf("__sysctl (%08x, %d, %08x, %d, %08x, %08x)\n", 
				frame[0], frame[1], 
				frame[2], *(uint32_t *)frame[3],
				frame[4], frame[5]);
			for (int i = 0; i < frame[1]; ++i) {
				printf("  %d\n", ((uint32_t *)frame[0])[i]);
			}
			call = ENOENT;
			return 0;
		case SYS_poll: {
			printf("poll (%08x, %d, %d)\n", frame[0], frame[1], frame[2]);
			int wait = frame[2];
			int lastsecond = last_time[1];
			while (wait > 0) {
				// enable interrupts, and wait for one; time keeping interrupts will move us forward
				asm volatile ( "sti; hlt" :: );
				if (last_time[1] != lastsecond) {
					lastsecond = last_time[1];
					wait -= 1000;
				}
			}
			call = 0;
			return 1;
			}
		case SYS_clock_gettime:
			printf("clock_gettime(%d, %08x)\n", frame[0], frame[1]);
			((struct timespec *) frame[1])->tv_sec = unix_time();
			((struct timespec *) frame[1])->tv_nsec = 0;
			call = 0;
			return 1;
		case SYS_issetugid:
			printf("issetugid()\n");
			call = 0;
			return 1;
		case SYS___getcwd:
			strlcpy((char *)frame[0], "/", frame[1]);
			call = 0;
			return 1;
		case SYS_sigprocmask:
			printf("sigprocmask (%d, ...)\n", frame[0]);
			call = 0;
			return 1;
		case SYS_sigaction:
			printf("sigaction (%d, ...)\n", frame[0]);
			call = 0;
			return 1;
		case SYS_thr_self:
			printf("thr_self()\n");
			*((long *)frame[0]) = 100002;
			call = 0;
			return 1;
		case SYS_thr_kill:
			printf("thr_kill(%d, %d)\n", frame[0], frame[1]);
			break;
			call = 0;
			return 1;
		case SYS_mmap:
			// round up to 4k page
			if (free_addr & 0x0FFF) {
				free_addr = (free_addr & 0xFFFFF000) + 4096;
			}
			if (frame[1] > (max_addr - free_addr + 1)) {
				printf("mmap (%08x, %08x, %08x, %08x, %d, %d) = ENOMEM\n",
					frame[0], frame[1], frame[2],
					frame[3], frame[4], frame[5]);
				call = ENOMEM;
				return 0;
			} else {
				printf("mmap (%08x, %08x, %08x, %08x, %d, %d) = %08x @ %08x\n",
					frame[0], frame[1], frame[2],
					frame[3], frame[4], frame[5],
					free_addr, frame[-1]);
				call = free_addr;
				free_addr += frame[1];
				return 1;
			}
		case SYS_openat: {
			printf("openat (%d, %s, %d)\n", frame[0], frame[1], frame[2]);
			struct hardcoded_file * file;
			file = find_file((char *)frame[1]);
			if (file != NULL) {
				FDS[next_fd].type = BOGFD_FILE;
				FDS[next_fd].data = file;
				FDS[next_fd].offset = file->start;
				call = next_fd;
				++next_fd;
				return 1;
			}
			call = ENOENT;
			return 0;
			}
		case SYS_pipe2:
			printf("pipe2 (%08x, %08x)\n", frame[0], frame[1]);
			FDS[next_fd].type = BOGFD_PIPE;
			FDS[next_fd + 1].type = BOGFD_PIPE;
			FDS[next_fd].data = &(FDS[next_fd + 1]);
			FDS[next_fd + 1].data = &(FDS[next_fd]);
			*((int *)frame[0]) = next_fd;
			*(((int *)frame[0]) + 1) = next_fd + 1;
			next_fd += 2;
			call = 0;
			return 1;
		case SYS_fstat:
			printf("fstat (%d)\n", frame[0]);
			if (frame[0] == 1) {
				struct stat* s = (struct stat*)frame[1];
				s->st_mode = S_IWUSR | S_IRUSR | S_IFCHR;
				call = 0;
				return 1;
			}
			break;
		case SYS_fstatat: {
			printf("fstatat (%d, %s, %d)\n", frame[0], frame[1], frame[2]);
			struct hardcoded_file * file;
			file = find_file((char *)frame[1]);
			if (file != NULL) {
				struct stat* s = (struct stat *)frame[2];
				s->st_dev = BOGFD_FILE;
				s->st_ino = (ino_t) file;
				s->st_nlink = 1;
				s->st_size = file->size;
				s->st_mode = S_IRUSR | S_IFREG;
				call = 0;
				return 1;
			}
			call = ENOENT;
			return 0;
			}
	}
				
	if (call < SYS_MAXSYSCALL) {
		printf("Got syscall %d (%s) (%08x, %08x) @%08x", call, syscallnames[call], frame[0], frame[1], frame[-1]);
	} else {
		printf("got unknown syscall %d", call);
	}
	//print_time(last_time);
	term_print ("halting\n");
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
		if (i < 0x20) {
			uint32_t handler = (uint32_t)(&unknown_int);
			handler +=(i * 5);
			IDT[i].offset_1 = handler & 0xFFFF;
			IDT[i].selector = 0x08;
			IDT[i].zero = 0;
			IDT[i].type_attr = 0x8E;
			IDT[i].offset_2 = handler >> 16;
		}
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
	//printf("loaded idtl of size 0x%04x\n", IDTR.size);
	asm volatile ( "sti" :: );
}

void *entrypoint;
void *phead_start;
size_t phent, phnum; 

void load_module(multiboot_module_t mod) {
	Elf32_Ehdr * head = (void *)mod.mod_start;
	entrypoint = (void *) head->e_entry;
	printf ("elf entrypoint 0x%08x\n", head->e_entry);
	phnum = head->e_phnum;
	phent = head->e_phentsize;
	Elf32_Phdr *phead = phead_start = (void *)mod.mod_start + head->e_phoff;

	printf ("%d program headers of size %d at %08x\n", phnum, phent, phead_start);
	
	for (int i = 0; i < head->e_phnum; ++i) {
		printf( "  %d: type 0x%x, offset %08x, virt %08x, filesize 0x%08x, memsize 0x%08x\n",
			i, phead->p_type, phead->p_offset, phead->p_vaddr,
			phead->p_filesz, phead->p_memsz);
		if (phead->p_type == PT_LOAD) {
			unsigned int count = min(phead->p_filesz, phead->p_memsz);
			uint8_t *src = (void*) mod.mod_start + phead->p_offset;
			uint8_t *dst = (void*) phead->p_vaddr;
			printf("copying %d bytes from %08x to %08x\n", count, src, dst);
			memcpy(dst, src, count);
			uint32_t next_addr = max(phead->p_filesz, phead->p_memsz) + phead->p_vaddr;
			if (next_addr > free_addr && next_addr < max_addr) {
				printf("moving free address from %08x to %08x\n", free_addr, next_addr);
				free_addr = next_addr;
			}
		}
		++phead;
	}
}

void enable_sse() {
	uint32_t a,b,c,d;
	uint32_t code = 1;
	asm volatile("cpuid":"=a"(a),"=b"(b),"=c"(c),"=d"(d):"a"(code));
	//printf("cpuid eax %08x, ebx %08x, ecx %08x, edx %08x\n", a, b, c, d);
	// https://wiki.osdev.org/SSE#Adding_support
	asm volatile("mov %%cr0, %0" : "=a" (a));
	a&= 0xFFFFFFFB;
	a|= 2;
	asm volatile("mov %0, %%cr0" :: "a"(a));
	asm volatile("mov %%cr4, %0" : "=a" (a));
	a|= (3 << 9);
	asm volatile("mov %0, %%cr4" :: "a"(a));
}

void setup_fds() 
{
	FDS[0].type = BOGFD_TERMIN;
	FDS[1].type = BOGFD_TERMOUT;
	FDS[2].type = BOGFD_TERMOUT;
	next_fd = 3;
	for (int i = next_fd; i < BOGFD_MAX; ++i) {
		FDS[i].type = BOGFD_CLOSED;
	}
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
	enable_sse();
	interrupt_setup();

	setup_fds();
	init_files();

	get_time(last_time);
	print_time(last_time);

	
	printf("memory map: %d @ %08x\n", mb->mmap_length, mb->mmap_addr);
      multiboot_memory_map_t *mmap;
      
      printf ("mmap_addr = 0x%x, mmap_length = 0x%x\n",
              (unsigned) mb->mmap_addr, (unsigned) mb->mmap_length);
      for (mmap = (multiboot_memory_map_t *) mb->mmap_addr;
           (unsigned long) mmap < mb->mmap_addr + mb->mmap_length;
           mmap = (multiboot_memory_map_t *) ((unsigned long) mmap
                                    + mmap->size + sizeof (mmap->size))) {
        printf (" size = 0x%x, base_addr = 0x%08x,"
                " length = 0x%08x, type = 0x%x\n",
                (unsigned) mmap->size,
                //(unsigned) (mmap->addr >> 32),
                (unsigned) (mmap->addr & 0xffffffff),
                //(unsigned) (mmap->len >> 32),
                (unsigned) (mmap->len & 0xffffffff),
                (unsigned) mmap->type);	
                if (mmap->type == 1) {
                	if (mmap->len > (max_addr - free_addr)) {
                		free_addr = mmap->addr;
                		max_addr = mmap->addr + (mmap->len - 1);
                		printf("free memory now at %08x-%08x\n", free_addr, max_addr);
			}
                }
	}

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
	
	
	
	if (entrypoint) {
		printf ("jumping to %08x\n", entrypoint);
		void* new_top = (void*) (max_addr & 0xFFFFFFFC);
		max_addr -= 1024 * 1024; // 1 MB stack should be good for now?

		// set up elf headers
		new_top -= sizeof(Elf32_Auxinfo);
		((Elf32_Auxinfo *)new_top)->a_type = AT_NULL;

		new_top -= sizeof(Elf32_Auxinfo);
		((Elf32_Auxinfo *)new_top)->a_type = AT_PHNUM;
		((Elf32_Auxinfo *)new_top)->a_un.a_val = phnum;

		new_top -= sizeof(Elf32_Auxinfo);
		((Elf32_Auxinfo *)new_top)->a_type = AT_PHENT;
		((Elf32_Auxinfo *)new_top)->a_un.a_val = phent;

		new_top -= sizeof(Elf32_Auxinfo);
		((Elf32_Auxinfo *)new_top)->a_type = AT_PHDR;
		((Elf32_Auxinfo *)new_top)->a_un.a_ptr = phead_start;

		// set up environment
		char * env[] = {"BINDIR=/", "ERL_INETRC=/cfg/inetrc", NULL };
		size_t bytes = sizeof(env);
		new_top -= bytes;
		memcpy (new_top, env, bytes);
		
		// set up arguments
		char *argv[] = {"beam",	"--", "-root", "",
		                "-progname", "erl", NULL};
		bytes = sizeof(argv);
		new_top -= bytes;
		memcpy (new_top, argv, bytes);
		new_top -= sizeof(char *);
		*(int *)new_top = (sizeof(argv) / sizeof (char*)) - 1;

		start_entrypoint(new_top, entrypoint);
	}
	while (1) {
		while (TIMER_COUNT) {
			outb(0x20,0x20);
			term_print(".");
			--TIMER_COUNT;
		}
	}
}

