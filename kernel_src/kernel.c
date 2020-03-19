#define USER_STACK_SIZE 1024 * 1024

#include "common.h"
#include "files.h"
#include "kern_mmap.h"
#include "bogfd.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>

extern const char *syscallnames[];
extern void * handle_int_80;
extern void * unknown_int;
extern void start_entrypoint(); 

extern void __executable_start, __etext, __data_start, __edata;

char **environ = {NULL};
char *__progname = "crazierlkernel";

char FS_TRANSFERRED = 0;

#define PORT_PIC1 0x20
#define PORT_COM1 0x3f8   /* COM1 */

#define PIC_INTERRUPT_ACK 0x20

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


#include <fcntl.h>
#include <sys/param.h>
#include <sys/mount.h>
#include <sys/dirent.h>
#include <sys/uio.h>
#include <poll.h>
#include <sys/rtprio.h>
#include <sys/umtx.h>
#include <sys/sysproto.h>
#include <stdarg.h>
#include <sys/cpuset.h>
#include <rtld_printf.h>
#include <x86intrin.h>

// include this last; use _KERNEL to avoid conflicting but unused definition
// of int sysarch between sysproto.h and sysarch.h

#define _KERNEL
#include <machine/sysarch.h>
#undef _KERNEL

struct BogusFD KERN_FDS[BOGFD_MAX];
size_t next_fd;

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

extern struct GDTDescr null_gdt;
extern struct GDTDescr ugs_base;

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

uintptr_t user_stack;

char INPUTBUFFER[16]; // must be a power of 2!
size_t INPUT_FD;



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

void move_cursor()
{
	const size_t index = (VGA_COLS * term_row) + term_col; // Like before, calculate the buffer index
	outb(0x3D4, 0x0F);
	outb(0x3D5, (uint8_t) (index & 0xFF));

	outb(0x3D4, 0x0E);
	outb(0x3D5, (uint8_t) ((index >> 8) & 0xFF));
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
	// enable cursor
	outb(0x3D4, 0x0A);
	outb(0x3D5, (inb(0x3D5) & 0xCE));

	outb(0x3D4, 0x0B);
	outb(0x3D5, (inb(0x3D5) & 0xEF));

	move_cursor();

	// init com port
	outb(PORT_COM1 + 1, 0x00);    // Disable all interrupts
	outb(PORT_COM1 + 3, 0x80);    // Enable DLAB (set baud rate divisor)
	outb(PORT_COM1 + 0, 0x03);    // Set divisor to 3 (lo byte) 38400 baud
	outb(PORT_COM1 + 1, 0x00);    //                  (hi byte)
	outb(PORT_COM1 + 3, 0x03);    // 8 bits, no parity, one stop bit
	outb(PORT_COM1 + 2, 0xC7);    // Enable FIFO, clear them, with 14-byte threshold
	outb(PORT_COM1 + 4, 0x0B);    // IRQs enabled, RTS/DSR set
	outb(PORT_COM1 + 1, 0x01);
}

int is_transmit_empty() {
   return inb(PORT_COM1 + 5) & 0x20;
}

void write_serial(char a) {
   while (is_transmit_empty() == 0);
 
   outb(PORT_COM1,a);
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
	case '\t': {
		if (term_col & 7) {
			term_col = (term_col & ~7) + 8;
		} else {
			term_col += 8;
		}
		break;
	}
	case 0x08: { // backspace
		if (term_col) { --term_col; }
		break;
	}
	default: // Normal characters just get displayed and then increment the column
		{
			if (c < 0x20 || c >= 0x7f) {
				//ERROR_PRINTF("unhandled control character %x\n", c);
			}
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
	move_cursor();
}

void term_printf(const char* format, ...)
{
       va_list args;
       va_start(args, format);

       char foo[512];
       rtld_vsnprintf(foo, sizeof(foo), format, args);
       term_print(foo);
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
	ERROR_PRINTF("%02d%02d-%02d-%02d %02d:%02d:%02d (%d)\n",
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
	ERROR_PRINTF("Got unexpected interrupt 0x%02x IP: %08x at ", irq, frame->ip);
	print_time(last_time);
	while (1) { }
}

void handle_unknown_error(struct interrupt_frame *frame, uint32_t irq, uint32_t error_code)
{
	ERROR_PRINTF("Got unexpected error %d (%d) IP: %08x at ", irq, error_code, frame->ip);
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
	}
	outb(PORT_PIC1, PIC_INTERRUPT_ACK);
	// clear RTC flag
	//outb(0x70, 0x0C);	// select register C
	//inb(0x71);		// just throw away contents
	++TIMER_COUNT;
}

void read_com(struct BogusFD * fd, uint16_t port)
{
	fd->status[3] &= ~ 0x01; // clear data pending flag
	while (inb(port + 5) & 1) {
		if (((fd->status[0] + 1) & fd->status[2]) != fd->status[1]) {
			uint8_t c = inb(port);
			if (c == '\r') { c = '\n'; }
			fd->buffer[fd->status[0]] = c;
			++fd->status[0];
			fd->status[0] &= fd->status[2];
		} else {
			ERROR_PRINTF("com 0x%x buffer full\n", port);
			fd->status[3] |= 0x01;
			return;
		}
	} 
}

__attribute__ ((interrupt))
void handle_com1(struct interrupt_frame *frame)
{
	struct BogusFD *fd = &KERN_FDS[INPUT_FD];
	if (fd->type == BOGFD_TERMIN) {
		read_com(fd, PORT_COM1);
	}
	outb(PORT_PIC1, PIC_INTERRUPT_ACK);
}

char unshifted_scancodes[] = {
	0, 0x1B,
	'1', '2', '3', '4', '5', '6', '7', '8',	'9', '0', '-', '=', 0x7F,
	'\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
	-4, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',
	-1, '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', -2,
	'*', -8, ' ' // ignore the rest of the keys
};

char shifted_scancodes[] = {
	0, 0x1B,
	'!', '@', '#', '$', '%', '^', '&', '*',	'(', ')', '_', '+', 0x7F,
	'\t', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', '\n',
	-4, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', '~',
	-1, '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', -2,
	'*', -8, ' ' // ignore the rest of the keys
};

uint8_t keyboard_shifts = 0;

void read_keyboard (struct BogusFD * fd) {
	fd->status[3] &= ~ 0x02; // clear data pending flag
	if (((fd->status[0] + 1) & fd->status[2]) != fd->status[1]) {
		uint8_t c = inb(0x60);
		if (c == 0xe0 || c== 0xe1) {
			// keyboard escape code; ignore for now
			return;
		}
		int down = 1;
		if (c & 0x80) {
			down = 0;
			c &= 0x7F;
		}
		if (c < sizeof(unshifted_scancodes)) {
			char o;
			if (keyboard_shifts & 0x3) {
				o = shifted_scancodes[c];
			} else {
				o = unshifted_scancodes[c];
			}
			if (o > 0) {
				if (down) {
					fd->buffer[fd->status[0]] = o;
					++fd->status[0];
					fd->status[0] &= fd->status[2];
				}
			} else {
				if (down) {
					keyboard_shifts |= (-o);
				} else {
					keyboard_shifts &= ~(-o);
				}
			}
		}
	} else {
		ERROR_PRINTF("keyboard buffer full\n");
		fd->status[3] |= 0x02;
		return;
	}
}

__attribute__ ((interrupt))
void handle_keyboard(struct interrupt_frame *frame)
{
	struct BogusFD *fd = &KERN_FDS[INPUT_FD];
	if (fd->type == BOGFD_TERMIN) {
		read_keyboard(fd);
	}
	outb(PORT_PIC1, PIC_INTERRUPT_ACK);
}

#define CARRY 1
#define SYSCALL_SUCCESS(ret) { iframe->flags &= ~CARRY; return ret; }
#define SYSCALL_FAILURE(ret) { iframe->flags |= CARRY; return ret; }

ssize_t write(int fd, const void * buf, size_t nbyte) {
	if (fd < BOGFD_MAX && (KERN_FDS[fd].type == BOGFD_TERMOUT || KERN_FDS[fd].type == BOGFD_TERMIN)) {
		uint8_t *buffer = (uint8_t *)buf;
		size_t nbytes = nbyte;
		while (nbytes) {
			_putchar(*buffer);
			++buffer;
			--nbytes;
		}
		move_cursor();
		return nbyte;
	} else {
		ERROR_PRINTF("write (%d, %08x, %d)\n", fd, buf, nbyte);
		return -EBADF;
	}
}

int handle_syscall(uint32_t call, struct interrupt_frame *iframe)
{	
	void *argp = (void *)(iframe->sp + sizeof(iframe->sp));
	switch(call) {
		case SYS_write: {
			struct write_args *a = argp;
			ssize_t ret = write(a->fd, a->buf, a->nbyte);
			if (ret >= 0) {
				SYSCALL_SUCCESS(ret);
			} else {
				SYSCALL_FAILURE(-ret);
			}
		}
		case SYS_writev: {
			struct writev_args *a = argp;
			if (a->fd < BOGFD_MAX && (KERN_FDS[a->fd].type == BOGFD_TERMOUT || KERN_FDS[a->fd].type == BOGFD_TERMIN)) {
				struct iovec *iov = a->iovp;
				unsigned int iovcnt = a->iovcnt;
				int written = 0;
				while (iovcnt) {
					uint8_t *buffer = (uint8_t *) iov->iov_base;
					size_t nbyte = iov->iov_len;
					written += nbyte;
					while (nbyte) {
						_putchar(*buffer);
						++buffer;
						--nbyte;
					}
					++iov;
					--iovcnt;
				}
				move_cursor();
				SYSCALL_SUCCESS(written);
			} else {
				ERROR_PRINTF("writev (%d, %08x, %d)\n", a->fd, a->iovp, a->iovcnt);
				SYSCALL_FAILURE(EBADF);
			}
		}
		case SYS_openat:
			argp += sizeof(argp);
		case SYS_open: {
			struct open_args *a = argp;
			if (FS_TRANSFERRED) {
				ERROR_PRINTF("open (%s, %08x) after FS transferred\n", a->path, a->flags);
				break;
			}
			char path[256];
			strlcpy (path, a->path, sizeof(path));
			if (strcmp(path, ".") == 0) {
				strcpy(path, "");
			}

			struct hardcoded_file * file;
			file = find_file(path);
			if (file != NULL) {
				KERN_FDS[next_fd].type = BOGFD_FILE;
				KERN_FDS[next_fd].file = file;
				KERN_FDS[next_fd].pos = file->start;
				DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
				SYSCALL_SUCCESS(next_fd++);
			}
			DEBUG_PRINTF ("open (%s, %08x) = ENOENT\n", path, a->flags);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_close: {
			struct close_args *a = argp;
			if (FS_TRANSFERRED) {
				ERROR_PRINTF("close (%d) after FS transferred\n", a->fd);
				break;
			}
			if (KERN_FDS[a->fd].type != BOGFD_CLOSED) {
				DEBUG_PRINTF("close (%d)\n", a->fd);
				KERN_FDS[a->fd].type = BOGFD_CLOSED;
				KERN_FDS[a->fd].file = NULL;
				KERN_FDS[a->fd].buffer = NULL;
				if (a->fd + 1 == next_fd) {
					--next_fd;
				}
				SYSCALL_SUCCESS(0);
			} else {
				ERROR_PRINTF("close (%d) = EBADF\n", a->fd);
				SYSCALL_FAILURE(EBADF);
			}
		}
		case SYS_getpid: SYSCALL_SUCCESS(2);
		case SYS_munmap: {
			struct munmap_args *a = argp;
			DEBUG_PRINTF("munmap (%08x, %d)\n",
				a->addr, a->len);
			kern_munmap(0, (uintptr_t) a->addr, a->len);
			SYSCALL_SUCCESS(0);
		}
		case SYS_mprotect: SYSCALL_SUCCESS(0); //ignore
		case SYS_madvise: SYSCALL_SUCCESS(0); //ignore
		case SYS_gettimeofday: {
			struct gettimeofday_args *a = argp;
			if (a->tp != NULL) {
				a->tp->tv_sec = unix_time();
				a->tp->tv_usec = 0;
			}
			if (a->tzp != NULL) {
				a->tzp->tz_minuteswest = 0;
				a->tzp->tz_dsttime = 0;
			}
			SYSCALL_SUCCESS(0);
		}
		case SYS_sysarch: {
			struct sysarch_args *a = argp;
			switch (a->op) {
				case I386_SET_GSBASE: {
					uint32_t base = *((uint32_t *) a->parms);
					ugs_base.base_1 = base & 0xFFFF;
					base >>= 16;
					ugs_base.base_2 = base & 0xFF;
					base >>= 8;
					ugs_base.base_3 = base & 0xFF;
					base = (uintptr_t)&ugs_base - (uintptr_t)&null_gdt;
					//DEBUG_PRINTF("Setting GS base to %08x, %d\n", a->parms, base);
					//asm volatile ( "movw %0, %%gs" :: "rm" (base));
					SYSCALL_SUCCESS(0);
				}
			}
		}
		case SYS_getrlimit: {
			struct __getrlimit_args *a = argp;
			switch (a->which) {
				case RLIMIT_STACK:
					a->rlp->rlim_cur = USER_STACK_SIZE;
					a->rlp->rlim_max = USER_STACK_SIZE;
					break;
				case RLIMIT_NOFILE:
					a->rlp->rlim_cur = BOGFD_MAX_USER;
					a->rlp->rlim_max = BOGFD_MAX_USER;
					break;
				default:
					a->rlp->rlim_cur = RLIM_INFINITY;
					a->rlp->rlim_max = RLIM_INFINITY;
			}
			SYSCALL_SUCCESS(0);
		}
		case SYS_setrlimit: {
			struct __setrlimit_args *a = argp;
			DEBUG_PRINTF("setrlimit (%d, {%d, %d})\n", a->which, a->rlp->rlim_cur, a->rlp->rlim_max);
			switch (a->which) {
				case RLIMIT_STACK:
					if (a->rlp->rlim_cur > USER_STACK_SIZE) {
						SYSCALL_FAILURE(EPERM);
					}
					SYSCALL_SUCCESS(0);
			}
			break;
		}
		case SYS___sysctl: {
			struct sysctl_args *a = argp;
			if (a->namelen == 2) {
				switch (a->name[0]) {
					case CTL_KERN: switch(a->name[1]) {
						case KERN_OSTYPE:
							strlcpy(a->old, "FreeBSD", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_OSRELEASE:
							strlcpy(a->old, "12.1-RELEASE-p1", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_VERSION:
							strlcpy(a->old, "FreeBSD 12.1-RELEASE-p1 GENERIC", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_HOSTNAME:
							strlcpy(a->old, "node0.crazierl.org", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_ARND:{
							uint8_t *r = (uint8_t *)a->old;
							uint32_t count = *a->oldlenp;
							DEBUG_PRINTF("random requested (%d)\n", count);
							ERROR_PRINTF("no reasonable random available\n");
							while (count) {
								/*unsigned int value;
								_rdrand32_step(&value); */
								*r = count & 0xFF;
								--count;
								++r;
							}
							SYSCALL_SUCCESS(0);
						}
						case KERN_OSRELDATE:
							*(u_int *)a->old = 1201000; // pretend to be freebsd 12.1 for now
							*a->oldlenp = sizeof(uint);
							SYSCALL_SUCCESS(0);
						case KERN_USRSTACK:
							*(uintptr_t *)a->old = user_stack;
							SYSCALL_SUCCESS(0);
					}
					case CTL_VM: switch (a->name[1]) {
						case VM_OVERCOMMIT:
							*(u_int *)a->old = 0;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
					}
					case CTL_HW: switch (a->name[1]) {
						case HW_MACHINE:
							strlcpy(a->old, "i386", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case HW_NCPU:
							DEBUG_PRINTF("hw.ncpu\n");
							*(u_int *)a->old = 1;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
						case HW_PAGESIZE:
							DEBUG_PRINTF("hw.pagesize\n");
							*(u_int *)a->old = PAGE_SIZE;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
					}
					case CTL_P1003_1B: switch (a->name[1]) {
						case CTL_P1003_1B_PAGESIZE:
							DEBUG_PRINTF("posix.pagesize\n");
							*(u_int *)a->old = PAGE_SIZE;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
					}
				}
			}
			ERROR_PRINTF("__sysctl (%08x, %d, %08x, %d, %08x, %d)\n",
				a->name, a->namelen,
				a->old, *a->oldlenp,
				a->new, a->newlen);
			for (int i = 0; i < a->namelen; ++i) {
				ERROR_PRINTF("  %d\n", a->name[i]);
			}
			SYSCALL_FAILURE(ENOENT);
		}
		/*case SYS_poll: {
			struct poll_args *a = argp;
			int waitleft = a->timeout;
			int lastsecond = last_time[1];
			int printed = 0;
			int changedfds = 0;
			while (waitleft > 0) {
				for (int i = 0; i < a->nfds; ++i) {
					if (a->fds[i].fd >= BOGFD_MAX) { continue; } // no EBADF?
					struct BogusFD *fd = &KERN_FDS[a->fds[i].fd];
					if (a->fds[i].events & POLLIN && fd->type == BOGFD_TERMIN && fd->status[0] != fd->status[1]) {
						a->fds[i].revents = POLLIN;
						++changedfds;
					}
				}
				if (changedfds) { SYSCALL_SUCCESS(changedfds); }
				if (!printed && a->timeout > 60000) {
					printed = 1;
					DEBUG_PRINTF("poll for %d fds, timeout %d\n", a->nfds, a->timeout);
					for (int i = 0; i < a->nfds; ++i) {
						DEBUG_PRINTF("  FD %d: events %08x\n", a->fds[i].fd, a->fds[i].events);
					}
				}
				// enable interrupts, and wait for one; time keeping interrupts will move us forward
				asm volatile ( "sti; hlt" :: );
				if (last_time[1] != lastsecond) {
					lastsecond = last_time[1];
					waitleft -= 1000;
				}
			}
			SYSCALL_SUCCESS(0);
		}*/
		case SYS_clock_gettime: {
			struct clock_gettime_args *a = argp;
			a->tp->tv_sec = unix_time();
			a->tp->tv_nsec = 0;
			SYSCALL_SUCCESS(0);
		}
		case SYS_issetugid:
			DEBUG_PRINTF("issetugid()\n");
			SYSCALL_SUCCESS(0);
		case SYS_sigprocmask: {
			struct sigprocmask_args *a = argp;
			DEBUG_PRINTF("sigprocmask (%d, %08x, %08x)\n", a->how, a->set, a->oset);
			SYSCALL_SUCCESS(0);
		}
		case SYS_sigaction: {
			struct sigaction_args *a = argp;
			DEBUG_PRINTF("sigaction (%d, %08x, %08x)\n", a->sig, a->act, a->oact);
			SYSCALL_SUCCESS(0);
		}
		case SYS_getcontext:
			ERROR_PRINTF("sending back bogus success for getcontext\n");
			SYSCALL_SUCCESS(0);
		case SYS_thr_self: {
			struct thr_self_args *a = argp;
			DEBUG_PRINTF("thr_self()\n");
			*a->id = 100002;
			SYSCALL_SUCCESS(0);
		}
		case SYS__umtx_op: {
			struct _umtx_op_args *a = argp;
			if (a->op == UMTX_OP_WAKE) {
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("_umtx_op(%08x, %d, %d, %08x, %08x)\n", a->obj, a->op, a->val, a->uaddr1, a->uaddr2);
		}
		case SYS_cpuset_getaffinity: {
			struct cpuset_getaffinity_args *a = argp;
			if (a->cpusetsize != sizeof(cpuset_t)) {
				ERROR_PRINTF("cpuset size %d, expecting %d\n", a->cpusetsize, sizeof(cpuset_t)) ;
				SYSCALL_FAILURE(ERANGE);
			}
			if (a->level == CPU_LEVEL_WHICH && a->which == CPU_WHICH_PID) {
				CPU_ZERO(a->mask);
				CPU_SET(0, a->mask);
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("cpuset_getaffinity(%d, %d, %llx, %d, %08x)\n", a->level, a->which, a->id, a->cpusetsize, a->mask);
			break;
		}
		case SYS_mmap: {
			struct mmap_args *a = argp;
			uintptr_t ret_addr;
			if (unlikely(a->fd == -2)) {
				ret_addr = transfer_files_to_userland();
				FS_TRANSFERRED = 1;
				SYSCALL_SUCCESS(ret_addr);
			} else if (kern_mmap(&ret_addr, a->addr, a->len, a->prot, a->flags)) {
				if (a->fd != -1 && a->fd < BOGFD_MAX && KERN_FDS[a->fd].type == BOGFD_FILE) {
					if (FS_TRANSFERRED) {
						ERROR_PRINTF("mmap on fd %d after FS transferred\n", a->fd);
						break;
					}
					if (a->pos == 0 && a->addr != NULL) { // && a->len > PAGE_SIZE) {
						ERROR_PRINTF("add-symbol-file %s -o 0x%08x\n", KERN_FDS[a->fd].file->name, a->addr);
					}

					if (a->pos + a->len > KERN_FDS[a->fd].file->size) {
						size_t avail = KERN_FDS[a->fd].file->size - a->pos;
						memcpy((void *)ret_addr, KERN_FDS[a->fd].file->start + a->pos, avail);
						explicit_bzero((void*)(ret_addr + avail), a->len - avail);
					} else {
						memcpy((void *)ret_addr, KERN_FDS[a->fd].file->start + a->pos, a->len);
					}
				} else if (a->prot != PROT_NONE) {
					explicit_bzero((void*)ret_addr, a->len);
				}
				SYSCALL_SUCCESS(ret_addr);
			} else {
				SYSCALL_FAILURE(ret_addr);
			}
		}
		/*case SYS_ppoll: {
			struct ppoll_args *a = argp;
			int waitleft = a->ts->tv_sec;
			int lastsecond = last_time[1];
			int printed = 0;
			int changedfds = 0;
			while (waitleft > 0) {
				for (int i = 0; i < a->nfds; ++i) {
					if (a->fds[i].fd >= BOGFD_MAX) { continue; } // no EBADF?
					struct BogusFD *fd = &KERN_FDS[a->fds[i].fd];
					if (a->fds[i].events & POLLIN && fd->type == BOGFD_TERMIN && fd->status[0] != fd->status[1]) {
						a->fds[i].revents = POLLIN;
						++changedfds;
					}
				}
				if (changedfds) { SYSCALL_SUCCESS(changedfds); }
				if (!printed) { // && a->ts->tv_sec > 60) {
					printed = 1;
					DEBUG_PRINTF("ppoll for %d fds, timeout %d\n", a->nfds, a->ts->tv_sec);
					for (int i = 0; i < a->nfds; ++i) {
						DEBUG_PRINTF("  FD %d: events %08x\n", a->fds[i].fd, a->fds[i].events);
					}
				}
				// enable interrupts, and wait for one; time keeping interrupts will move us forward
				asm volatile ( "sti; hlt" :: );
				if (last_time[1] != lastsecond) {
					lastsecond = last_time[1];
					waitleft -= 1;
				}
			}
			SYSCALL_SUCCESS(0);
		
		}*/
		case SYS_fstat: {
			struct fstat_args *a = argp;
			if (FS_TRANSFERRED) {
				ERROR_PRINTF("fstat on fd %d after FS transferred\n", a->fd);
				break;
			}
			if (a->fd < BOGFD_MAX && (KERN_FDS[a->fd].type == BOGFD_TERMIN || KERN_FDS[a->fd].type == BOGFD_TERMOUT)) {
				explicit_bzero(a->sb, sizeof(*a->sb));
				a->sb->st_mode = S_IWUSR | S_IRUSR | S_IFCHR;
				SYSCALL_SUCCESS(0);
			} else if (a->fd < BOGFD_MAX && KERN_FDS[a->fd].type == BOGFD_FILE) {
				explicit_bzero(a->sb, sizeof(*a->sb));
				struct BogusFD * fd = &KERN_FDS[a->fd];
				a->sb->st_dev = BOGFD_FILE;
				a->sb->st_ino = (ino_t) fd->file;
				a->sb->st_nlink = 1;
				a->sb->st_size = fd->file->size;
				a->sb->st_mode = S_IRUSR | S_IFREG | S_IRWXU;
				SYSCALL_SUCCESS(0);
			}
			DEBUG_PRINTF("fstat (%d)\n", a->fd);
			break;
		}
		case SYS_fstatfs: {
			struct fstatfs_args *a = argp;
			if (FS_TRANSFERRED) {
				ERROR_PRINTF("fstatfs on fd %d after FS transferred\n", a->fd);
				break;
			}
			if (a->fd < BOGFD_MAX && (KERN_FDS[a->fd].type == BOGFD_DIR || KERN_FDS[a->fd].type == BOGFD_FILE)) {
				bzero(a->buf, sizeof(struct statfs));
				a->buf->f_version = STATFS_VERSION;
				strlcpy(a->buf->f_fstypename, "BogusFS", sizeof(a->buf->f_fstypename));
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("fstatfs (%d)\n", a->fd);
			SYSCALL_FAILURE(EBADF);
		}
	}
				
	if (call < SYS_MAXSYSCALL) {
		unsigned int *args = argp;
		ERROR_PRINTF("Got syscall %d (%s) (%08x, %08x) @%08x", call, syscallnames[call], args[0], args[1], args[-1]);
	} else {
		ERROR_PRINTF("got unknown syscall %d", call);
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
		ERROR_PRINTF("Got #GP (%u) IP: %08x at ", error_code, frame->ip);
	} else {
		ERROR_PRINTF("Got #GP, no stack frame\n");
	}
	print_time(last_time);
	while (1) { } // loop forever
}

__attribute__ ((interrupt))
void handle_pf(struct interrupt_frame *frame, uint32_t error_code)
{
	uint32_t addr;
	asm volatile("mov %%cr2, %0" : "=a" (addr));

	if (frame) {
		kern_mmap_debug(addr);
		ERROR_PRINTF("Got #PF (%u) address %08x, IP: %08x at ", error_code, addr, frame->ip);
	} else {
		ERROR_PRINTF("Got #PF, no stack frame\n");
	}
	print_time(last_time);
	while (1) { } // loop forever
}

__attribute__ ((interrupt))
void handle_ud(struct interrupt_frame *frame)
{
	ERROR_PRINTF("Got #UD IP: %08x at ", frame->ip);
	print_time(last_time);
	while (1) { } // loop forever
}
void interrupt_setup()
{
	// remap primary PIC so that the interrupts don't conflict with Intel exceptions
	
	uint8_t mask = inb(0x21); // read current mask from PIC
	mask &= (~0x01); // enable irq 0 -> PIT timer
	mask &= (~0x10); // enable irq 4 -> COM1

	outb(PORT_PIC1, 0x11); // request initialization
	outb(0x80, 0); // wait cycle
	outb(PORT_PIC1 + 1, 0x20); // offset interrupts by 0x20
	outb(0x80, 0); // wait
	outb(PORT_PIC1 + 1, 0x4); // indicate slave PIC on IRQ 2
	outb(0x80, 0); //wait
	outb(PORT_PIC1 + 1, 0x01); // set to 8086 mode
	outb(0x80, 0); //wait
	outb(PORT_PIC1 + 1, mask); // reset interrupt mask

	// ensure IDT entries are not present
	for (int i = 0; i < (sizeof(IDT) / sizeof(IDT[0])); ++i) {
		IDT[i].type_attr = 0;
		uint32_t handler = (uint32_t)(&unknown_int);
		handler +=(i * 5);
		IDT[i].offset_1 = handler & 0xFFFF;
		IDT[i].selector = 0x08;
		IDT[i].zero = 0;
		IDT[i].type_attr = 0x8E;
		IDT[i].offset_2 = handler >> 16;
	}

	IDT[0x06].offset_1 = ((uint32_t) &handle_ud) & 0xFFFF;
	IDT[0x06].offset_2 = ((uint32_t) &handle_ud) >> 16;

	IDT[0x0D].offset_1 = ((uint32_t) &handle_gp) & 0xFFFF;
	IDT[0x0D].offset_2 = ((uint32_t) &handle_gp) >> 16;

	IDT[0x0E].offset_1 = ((uint32_t) &handle_pf) & 0xFFFF;
	IDT[0x0E].offset_2 = ((uint32_t) &handle_pf) >> 16;

	IDT[0x20].offset_1 = ((uint32_t) &handle_timer) & 0xFFFF;
	IDT[0x20].offset_2 = ((uint32_t) &handle_timer) >> 16;

	IDT[0x21].offset_1 = ((uint32_t) &handle_keyboard) & 0xFFFF;
	IDT[0x21].offset_2 = ((uint32_t) &handle_keyboard) >> 16;

	IDT[0x24].offset_1 = ((uint32_t) &handle_com1) & 0xFFFF;
	IDT[0x24].offset_2 = ((uint32_t) &handle_com1) >> 16;

	IDT[0x80].offset_1 = ((uint32_t) &handle_int_80) & 0xFFFF;
	IDT[0x80].offset_2 = ((uint32_t) &handle_int_80) >> 16;
	IDT[0x80].type_attr = 0xEE; // allow all rings to call in
	
	IDTR.size = sizeof(IDT) - 1;
	IDTR.offset = (uint32_t) &IDT;
	asm volatile ( "lidt %0" :: "m" (IDTR) );
	//ERROR_PRINTF("loaded idtl of size 0x%04x\n", IDTR.size);
	asm volatile ( "sti" :: );
}

void *entrypoint;
void *phead_start;
size_t phent, phnum; 
uintptr_t load_addr;

void load_file(void *start, char *name, size_t size)
{
	Elf32_Ehdr * head = (Elf32_Ehdr *) start;
	entrypoint = (void *) head->e_entry;
	DEBUG_PRINTF ("elf entrypoint 0x%08x\n", head->e_entry);
	phnum = head->e_phnum;
	phent = head->e_phentsize;
	Elf32_Phdr *phead = phead_start = start + head->e_phoff;

	DEBUG_PRINTF ("program binary size %d (0x%08x - 0x%08x)\n", size, start, start + size);
	DEBUG_PRINTF ("%d program headers of size %d at %08x\n", phnum, phent, phead_start);

	uintptr_t first_addr = -1;
	uintptr_t first_virtual = -1;
	uintptr_t last_addr = 0;
	uintptr_t last_virtual = 0;

	for (int i = 0; i < head->e_phnum; ++i) {
		if (phead->p_type == PT_LOAD) {
			first_addr = min (first_addr, phead->p_offset);
			first_virtual = min (first_virtual, phead->p_vaddr);
			last_addr = max (last_addr, phead->p_offset + phead->p_memsz);
			last_virtual = max (last_virtual, phead->p_vaddr + phead->p_memsz);
		}
		++phead;
	}

	ERROR_PRINTF("load offsets %08x - %08x; virt %08x - %08x\n", first_addr, last_addr, first_virtual, last_virtual);
	load_addr = 0;
/*	if (0) { // kern_mmap_could_map(first_virtual, last_virtual)) {


	} else if (size >= last_addr) {
		DEBUG_PRINTF("using existing file memory to load\n");
		load_addr = (uintptr_t) start;
	} else { */
		if (!kern_mmap(&load_addr, 0, last_addr - first_addr, PROT_KERNEL | PROT_READ | PROT_WRITE, 0)){
			ERROR_PRINTF("couldn't map to load initial executable\n");
		}
		explicit_bzero((void *) load_addr, last_addr - first_addr);
//	}
	if (load_addr != first_virtual) {
		entrypoint = entrypoint - first_virtual + load_addr;
		ERROR_PRINTF("elf entrypoint moved to 0x%08x\n", entrypoint);
	}

	ERROR_PRINTF("add-symbol-file %s -o 0x%08x\n", name, load_addr);

	phead = phead_start;
	size_t lastoffset = 0;
	for (int i = 0; i < head->e_phnum; ++i) {
		if (phead->p_type == PT_LOAD) {
			DEBUG_PRINTF( "  %d: PT_LOAD offset %08x, virt %08x, filesize 0x%08x, memsize 0x%08x\n",
				i, phead->p_type, phead->p_offset, phead->p_vaddr,
				phead->p_filesz, phead->p_memsz);
			if (phead->p_offset < lastoffset) {
				ERROR_PRINTF("elf header %d has p_offset > last_offset; halting\n", i);
				while (1) {}
			}
			if (phead->p_offset > lastoffset) {
				size_t count = phead->p_offset - lastoffset;
				ERROR_PRINTF("zeroing %d bytes from %08x to %08x\n", count, load_addr + lastoffset, load_addr + lastoffset + count);
				explicit_bzero((uint8_t *)(load_addr + lastoffset), count);
			}

			if (phead->p_filesz > phead->p_memsz) {
				ERROR_PRINTF("elf header %d has p_filesz > p_memsz; halting\n", i);
				while (1) { }
			}
			uint8_t *src = start + phead->p_offset;
			uint8_t *dst = (void*) (load_addr - first_virtual +  phead->p_vaddr);
			if (src != dst) {
				ERROR_PRINTF("copying %d bytes from %08x to %08x\n", phead->p_filesz, src, dst);
				memcpy(dst, src, phead->p_filesz);
			}
			lastoffset = phead->p_offset + phead->p_filesz;
			uint32_t scratch;
			if (!kern_mmap(&scratch, (void *)(load_addr + phead->p_vaddr), phead->p_memsz, PROT_READ|PROT_WRITE, 0)) {
				ERROR_PRINTF("couldn't map ELF load section %08x\n", load_addr + phead->p_vaddr);
			}
		}
		++phead;
	}
	size_t count = size - lastoffset;
	if (count) {
		ERROR_PRINTF("zeroing final %d bytes from %08x to %08x\n", count, lastoffset + start, start + size);
		explicit_bzero((uint8_t*) (start + lastoffset), count);
	}
	kern_munmap(PROT_KERNEL, load_addr, last_virtual - first_virtual);
}

void enable_sse() {
	uint32_t a,b,c,d;
	uint32_t code = 1;
	asm volatile("cpuid":"=a"(a),"=b"(b),"=c"(c),"=d"(d):"a"(code));
	//ERROR_PRINTF("cpuid eax %08x, ebx %08x, ecx %08x, edx %08x\n", a, b, c, d);
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
	KERN_FDS[0].type = BOGFD_TERMIN;
	KERN_FDS[0].buffer = INPUTBUFFER;
	KERN_FDS[0].status[0] = 0;
	KERN_FDS[0].status[1] = 0;
	KERN_FDS[0].status[2] = sizeof(INPUTBUFFER) -1; // hope this is a power of two
	
	INPUT_FD = 0;
	KERN_FDS[1].type = BOGFD_TERMOUT;
	KERN_FDS[2].type = BOGFD_TERMOUT;
	next_fd = 3;
	for (int i = next_fd; i < BOGFD_MAX; ++i) {
		KERN_FDS[i].type = BOGFD_CLOSED;
	}
}

void setup_entrypoint()
{
	struct hardcoded_file * file = find_file("/beam");
	if (!file) {
		ERROR_PRINTF("couldn't find /beam\n");
	}
	KERN_FDS[next_fd].type = BOGFD_FILE;
	KERN_FDS[next_fd].file = file;
	KERN_FDS[next_fd].pos = file->start;
	++next_fd;
	if (!kern_mmap(&user_stack, NULL, USER_STACK_SIZE, PROT_WRITE | PROT_READ, MAP_STACK | MAP_ANON)) {
		ERROR_PRINTF("couldn't get map for user stack\n");
		while (1) {}
	}
	user_stack += USER_STACK_SIZE; // we actually want to keep track of the top of the stack

	void* new_top = (void*) user_stack;

	// list of page sizes
	new_top -= sizeof(int);
	*(int *)new_top = 0;
	new_top -= sizeof(int);
	*(int *)new_top = 0x1000;
	void * page_sizes = new_top;

	// set up environment
	char * env[] = {"BINDIR=/", "ERL_INETRC=/cfg/inetrc",
		"TERM=vt100",
		"LD_32_PRELOAD=/obj/libuserland.so",
		//"LD_32_DEBUG=1",
		NULL};
	// set up arguments
	char *argv[] = {"/beam", "--", "-root", "",
			"-progname", "erl", "--", "-home", "/",
			"--", NULL};

	int i = 0;
	while (env[i] != NULL) {
		size_t len = strlen(env[i]) + 1;
		new_top -= len;
		memcpy ((uint8_t*) new_top, env[i], len);
		env[i] = new_top;
		++i;
	}
	i = 0;
	while (argv[i] != NULL) {
		size_t len = strlen(argv[i]) + 1;
		new_top -= len;
		memcpy ((uint8_t*) new_top, argv[i], len);
		argv[i] = new_top;
		++i;
	}

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

	new_top -= sizeof(Elf32_Auxinfo);
	((Elf32_Auxinfo *)new_top)->a_type = AT_BASE;
	((Elf32_Auxinfo *)new_top)->a_un.a_ptr = (void *)load_addr;

	new_top -= sizeof(Elf32_Auxinfo);
	((Elf32_Auxinfo *)new_top)->a_type = AT_PAGESIZESLEN;
	((Elf32_Auxinfo *)new_top)->a_un.a_val = 8;

	new_top -= sizeof(Elf32_Auxinfo);
	((Elf32_Auxinfo *)new_top)->a_type = AT_PAGESIZES;
	((Elf32_Auxinfo *)new_top)->a_un.a_ptr = page_sizes;

	new_top -= sizeof(Elf32_Auxinfo);
	((Elf32_Auxinfo *)new_top)->a_type = AT_EXECFD;
	((Elf32_Auxinfo *)new_top)->a_un.a_val = next_fd -1;


	// copy environment pointers
	size_t bytes = sizeof(env);
	new_top -= bytes;
	memcpy (new_top, env, bytes);

	// copy argv pointers
	bytes = sizeof(argv);
	new_top -= bytes;
	memcpy (new_top, argv, bytes);
	new_top -= sizeof(char *);
	*(int *)new_top = (sizeof(argv) / sizeof (char*)) - 1;

	DEBUG_PRINTF ("jumping to %08x\n", entrypoint);
	start_entrypoint(new_top, entrypoint);
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

	get_time(last_time);
	print_time(last_time);
	
	kern_mmap_init(mb->mmap_length, mb->mmap_addr);
	
	uintptr_t scratch;
	ERROR_PRINTF("kernel read-only %08x - %08x\n", &__executable_start, &__etext);

	if (!kern_mmap(&scratch, &__executable_start, &__etext - &__executable_start, PROT_KERNEL | PROT_READ, 0)) {
		ERROR_PRINTF("couldn't map read only kernel section\n");
		while (1) { }
	}

	ERROR_PRINTF("kernel read-write %08x - %08x\n", &__data_start, &__edata);
	if (!kern_mmap(&scratch, &__data_start, &__edata - &__data_start, PROT_KERNEL | PROT_READ | PROT_WRITE, 0)) {
		ERROR_PRINTF("couldn't map read/write kernel section\n");
		while (1) { }
	}
	
	if (!kern_mmap(&scratch, (void *)vga_buffer, 80 * 25 * 2, PROT_KERNEL | PROT_FORCE | PROT_READ | PROT_WRITE, 0)) {
		ERROR_PRINTF("couldn't map vga buffer\n");
	}
	
	DEBUG_PRINTF("kernel main at %08x\n", kernel_main);
	DEBUG_PRINTF("Multiboot magic: %08x (%s)\n", mb_magic, (char *) mb->boot_loader_name);
	DEBUG_PRINTF("Multiboot info at %08x (%08x)\n", mb, &mb);
	DEBUG_PRINTF("mem range: %08x-%08x\n", mb->mem_lower, mb->mem_upper);
	DEBUG_PRINTF("modules: %d @ %08x\n", mb->mods_count, mb->mods_addr);
	
	DEBUG_PRINTF("command line: %s\n", mb->cmdline);
	char * filestart = strchrnul((char *)mb->cmdline, ' ');
	while (*filestart == ' ') { ++filestart; }
	char * fileend = strchrnul(filestart, ' ');
	char filename [256];
	strncpy(filename, filestart, fileend - filestart);
	DEBUG_PRINTF("file to load %s\n", filename);

	size_t mods_count = mb->mods_count;
	multiboot_module_t *mods = (void *)mb->mods_addr;

	kern_mmap_enable_paging();
	kern_mmap(&scratch, (void *) mods, mods_count * sizeof(mods), PROT_KERNEL | PROT_READ | PROT_FORCE, 0);

	for (int mod = 0; mod < mods_count; ++mod) {
		DEBUG_PRINTF("Module %d (%s):\n 0x%08x-0x%08x\n", mod, mods[mod].cmdline, mods[mod].mod_start, mods[mod].mod_end);
		init_files(&mods[mod]);
	}
	
	struct hardcoded_file * file = find_file(filename);
	if (file) {
		DEBUG_PRINTF("loading %s at %08x\n", filename, file->start);
		load_file(file->start, file->name, file->size);
	}

	if (entrypoint) {
		setup_entrypoint();
	}
	while (1) {
		while (TIMER_COUNT) {
			term_print(".");
			--TIMER_COUNT;
		}
	}
}
