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
extern void * pic1_int;
extern void * stack_top;
extern void start_entrypoint();
extern uintptr_t setup_new_stack(uintptr_t stack_pointer);
extern int switch_thread_impl(uintptr_t* oldstack, uintptr_t newstack);
extern void __executable_start, __etext, __data_start, __edata;
extern uintptr_t tss_esp0;

char **environ = {NULL};
char *__progname = "crazierlkernel";

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
#include <sys/socket.h>
#include <sys/thr.h>
#include "threads.h"

// include this last; use _KERNEL to avoid conflicting but unused definition
// of int sysarch between sysproto.h and sysarch.h

#define _KERNEL
#include <machine/sysarch.h>
#undef _KERNEL

struct BogusFD FDS[BOGFD_MAX];
size_t next_fd;

uint8_t WANT_NMI = 0;
 
#define MAX_THREADS 32
#define IDLE_THREAD (MAX_THREADS - 1)
#define THREAD_ID_OFFSET 100002
struct crazierl_thread threads[MAX_THREADS];
volatile size_t current_thread = 0;
size_t next_thread = 1;

// This is the x86's VGA textmode buffer. To display text, we write data to this memory location
#define VGA_BUFFER_SIZE 0x20000
#define VGA_BUFFER_ELEMENTS (VGA_BUFFER_SIZE / sizeof(vga_buffer[0]))
volatile uint16_t *vga_buffer = (uint16_t*)0xA0000;
// By default, the VGA textmode buffer has a size of 80x25 characters
#define VGA_COLS 80
#define VGA_MEM_COLS 128
#define VGA_ROWS 25

int vga_current_index = 0;
 
// We start displaying text in the top-left of the screen (column = 0, row = 0)
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

uint64_t fixed_point_time;
#define FIXED_POINT_TIME_NANOSECOND(seconds, nanoseconds) (((uint64_t) seconds << 24) + (((uint64_t) nanoseconds << 24) / 1000000000))
#define FIXED_POINT_SECONDS(fpt)(fpt >> 24)
#define FIXED_POINT_MILLISECONDS(fpt) (((fpt & ((1 << 24) -1)) * 1000) >> 24)
#define FIXED_POINT_NANOSECONDS(fpt) (((fpt & ((1 << 24) -1)) * 1000000000) >> 24)

uintptr_t user_stack;

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

void find_cursor()
{
	outb(0x3D4, 0x0F);
	vga_current_index = inb(0x3D5);

	outb(0x3D4, 0x0E);
	vga_current_index |= inb(0x3d5) << 8;
}

void move_cursor()
{
	outb(0x3D4, 0x0F);
	outb(0x3D5, (uint8_t) (vga_current_index & 0xFF));

	outb(0x3D4, 0x0E);
	outb(0x3D5, (uint8_t) ((vga_current_index >> 8) & 0xFF));
}
 
// This function initiates the terminal by clearing it
void term_init()
{
	// setup (horizontal) output offset
	outb(0x3D4, 0x13);
	outb(0x3D5, VGA_MEM_COLS / 2);

	// select 0xA0000 - 0xBFFFF memory range
	outb(0x3CE, 0x06);
	uint8_t misc_gfx = inb(0x3CF);
	misc_gfx &= 0xF3; // clear bits 2 and 3
	outb(0x3CF, misc_gfx);

	// Clear the textmode buffer
	for (int i = 0; i < VGA_BUFFER_ELEMENTS; ++i) {
		vga_buffer[i] = ((uint16_t)term_color << 8) | ' '; // Set the character to blank (a space character)
	}
	vga_current_index = VGA_BUFFER_ELEMENTS - (70 * VGA_MEM_COLS); // set to trigger wrapparound
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
	int endofline = 0;
	// Remember - we don't want to display ALL characters!
	switch (c)
	{
	case '\r': break;
	case '\n': // Newline characters should return the column to 0, and increment the row
		{
			endofline = 1;
			break;
		}
	case '\t': {
		if (vga_current_index & 7) {
			vga_current_index = ((vga_current_index & ~7) + 8);
		} else {
			vga_current_index+= 8;
		}
		break;
	}
	case 0x08: { // backspace
		if (vga_current_index & (VGA_MEM_COLS - 1)) {
			--vga_current_index;
		}
		break;
	}
	default: // Normal characters just get displayed and then increment the column
		{
			if (c < 0x20 || c >= 0x7f) {
				//ERROR_PRINTF("unhandled control character %x\n", c);
			}
			vga_buffer[vga_current_index] = ((uint16_t)term_color << 8) | c;
			/*if (vga_current_index + VGA_SCREEN >= VGA_BUFFER_SIZE) {
				vga_buffer[vga_current_index - (VGA_BUFFER_SIZE - VGA_SCREEN)] = ((uint16_t)term_color << 8) | c;
			}*/
			++vga_current_index;
			break;
		}
	}
 
	// What happens if we get past the last column? We need to reset the column to 0, and increment the row to get to a new line
	if ((vga_current_index & (VGA_MEM_COLS - 1)) >= VGA_COLS)
	{
		endofline = 1;
	}
 
	// What happens if we get past the last row? We need to reset both column and row to 0 in order to loop back to the top of the screen
	if (endofline) {
		vga_current_index = (vga_current_index & ~(VGA_MEM_COLS - 1)) + VGA_MEM_COLS;
		if (vga_current_index == VGA_BUFFER_ELEMENTS) {
			vga_current_index = 0;
		}
		// clear the line if we're starting a new line
		for (int i = 0; i < VGA_COLS; ++i) {
			vga_buffer[(vga_current_index + i) % VGA_BUFFER_ELEMENTS] = ((uint16_t)term_color << 8) | ' ';
		}
		int index = vga_current_index - (VGA_MEM_COLS * (VGA_ROWS -1));

		static int last_line_compare = 0x3FF;
		int line_compare = 0x3FF;
		if (index < 0) {
			line_compare = 16 * (-index / VGA_MEM_COLS);
			index += VGA_BUFFER_ELEMENTS;
		}
		if (line_compare != last_line_compare) {
			outb(0x3D4, 0x07);
			uint8_t tmp = inb(0x3D5);
			uint8_t out;
			if (line_compare & 0x100) {
				out = tmp | 0x10;
			} else {
				out = tmp & ~0x10;
			}
			if (out != tmp) {
				outb(0x3D5, out);
			}
			outb(0x3D4, 0x09);
			tmp = inb(0x3D5);
			if (line_compare & 0x200) {
				out = tmp | 0x40;
			} else {
				out = tmp & ~ 0x40;
			}
			if (out != tmp) {
				outb(0x3D5, out);
			}
			outb(0x3D4, 0x18);
			outb(0x3D5, line_compare & 0xFF);
			last_line_compare = line_compare;
		}
		outb(0x3D4, 12);
		outb(0x3D5, index >> 8);
		outb(0x3D4, 13);
		outb(0x3D5, index & 0xFF);
	}
}
 
// This function prints an entire string onto the screen
void term_print(const char* str)
{
	for (size_t i = 0; str[i] != '\0'; i ++) // Keep placing characters until we hit the null-terminating character ('\0')
		_putchar(str[i]);
	move_cursor();
}

void term_printn (const uint8_t* str, ssize_t len)
{
	while (len) {
		_putchar(*str);
		++str;
		--len;
	}
	move_cursor();
}

void term_printf(const char* format, ...)
{
       va_list args;
       va_start(args, format);

       char foo[512];
       rtld_vsnprintf(foo, sizeof(foo), format, args);
       write(2, foo, strlen(foo));
}

_Noreturn
void halt(char * message) {
	find_cursor();
	if (message) {
		term_print(message);
	}
	for (int i = 2; i >= 0; --i) {
		if (FDS[i].type == BOGFD_PIPE) {
			term_printn(FDS[i].pipe->pb->data, FDS[i].pipe->pb->length);
		}
	}
	asm volatile ( "hlt" :: );
	while (1) { }
}

uint8_t read_cmos(uint8_t reg)
{
	outb(0x70, WANT_NMI | reg);
	return inb(0x71);
}

uint64_t unix_time(uint8_t time[]) {
	int years = (time[7] * 100 + time[6]) - 1970;
	int days = years * 365 + (years >> 2) + time[4];
	if (time[5] == 1 || (time[5] == 2 && time[4] <= 29)) {
		--days;
	}
	switch (time[5]) {
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
	return days * 86400 + time[3] * 3600 + time[2] * 60 + time[1];
}

void get_time()
{
	uint8_t timeA[9], timeB[9];
	bzero(timeA, sizeof(timeA));
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
	fixed_point_time = FIXED_POINT_TIME_NANOSECOND(unix_time(timeA), 0);
}

	
int switch_thread(unsigned int new_state, uint64_t timeout) {
	size_t old_thread = current_thread;
	size_t i = old_thread + 1;
	size_t target = old_thread;
	int timed_out = 0;
	if (i == MAX_THREADS) { i = 0; }
	if (timeout && timeout <= fixed_point_time) {
		threads[current_thread].state = THREAD_RUNNING;
		return 1; // don't switch if it's already past the time
	}
	while (i != old_thread) {
		if (threads[i].state == THREAD_RUNNABLE) {
			target = i;
			break;
		} else if (threads[i].timeout && threads[i].timeout <= fixed_point_time) {
			target = i;
			timed_out = 1;
			break;
		}
		++i;
		if (i == MAX_THREADS) { i = 0; }
	}
	if (target == old_thread) {
		if (new_state == THREAD_RUNNABLE) {
			return 0;
		} else {
			target = IDLE_THREAD;
		}
	}
	DEBUG_PRINTF("switching from %d (%d) to %d\n", old_thread, new_state, target);
	DEBUG_PRINTF("new stack %p of %p\n",
		threads[target].kern_stack_cur, threads[target].kern_stack_top);
	DEBUG_PRINTF("old stack %p of %p\n",
		threads[old_thread].kern_stack_cur, threads[old_thread].kern_stack_top);
	current_thread = target;

	if (threads[old_thread].state != THREAD_IDLE) {
		threads[old_thread].state = new_state;
	}
	if (timeout) {
		threads[old_thread].timeout = timeout;
	}
	asm volatile ( "fxsave (%0)" :: "a"(&threads[old_thread].savearea) :);
	if (threads[target].state != THREAD_IDLE) {
		threads[target].state = THREAD_RUNNING;
		uintptr_t base = threads[target].tls_base;

		ugs_base.base_1 = base & 0xFFFF;
		base >>= 16;
		ugs_base.base_2 = base & 0xFF;
		base >>= 8;
		ugs_base.base_3 = base & 0xFF;
		tss_esp0 = threads[target].kern_stack_top;
		if (timed_out) {
			*(int *)threads[target].kern_stack_cur = 1;
		}
	}
	int we_timed_out = switch_thread_impl(&threads[old_thread].kern_stack_cur, threads[target].kern_stack_cur);
	asm volatile ( "fxrstor (%0)" :: "a"(&threads[current_thread].savearea) :);
	threads[current_thread].timeout = 0;
	return we_timed_out;
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
	halt(NULL);
}

int UNCLAIMED_PIC_INT = 0;
void handle_pic1_irq(struct interrupt_frame *frame, uint32_t irq)
{
	outb(PORT_PIC1, PIC_INTERRUPT_ACK);
	int found = 0;
	for (int fd = 0; fd < BOGFD_MAX; ++fd) {
		if (FDS[fd].type == BOGFD_PIC1 && FDS[fd].status[0] == irq) {
			++found;
			write(fd, "!", 1);
		}
	}
	if (!found && !UNCLAIMED_PIC_INT) {
		UNCLAIMED_PIC_INT = 1;
		ERROR_PRINTF("didn't find FD for pic interrupt %d\n", irq);
	}
}

void handle_unknown_error(struct interrupt_frame *frame, uint32_t irq, uint32_t error_code)
{
	ERROR_PRINTF("Got unexpected error %d (%d) IP: %08x at ", irq, error_code, frame->ip);
	halt(NULL);
}

__attribute__ ((interrupt))
void handle_timer(struct interrupt_frame *frame)
{
	outb(PORT_PIC1, PIC_INTERRUPT_ACK);
	// clear RTC flag
	//outb(0x70, 0x0C);	// select register C
	//inb(0x71);		// just throw away contents
	++TIMER_COUNT;
	fixed_point_time += FIXED_POINT_TIME_NANOSECOND(0, 54925438); // PIT timer is 54.92... ms
	switch_thread(THREAD_RUNNABLE, 0);
}

ssize_t write(int fd, const void * buf, size_t nbyte) {
	struct BogusFD *wait_target = NULL;
	int written;
	if (fd < 0 || fd >= BOGFD_MAX) {
		return -EBADF;
	}
	if (FDS[fd].type == BOGFD_TERMOUT || FDS[fd].type == BOGFD_TERMIN) {
		term_printn(buf, nbyte);
		written = nbyte;
	} else if (FDS[fd].type == BOGFD_PIPE) {
		if (FDS[fd].pipe == NULL) {
			return -EPIPE;
		} else {
			written = min(nbyte, BOGFD_PB_LEN - FDS[fd].pipe->pb->length);
			//ERROR_PRINTF("write (%d, \"%s\", %d) = %d\n", fd, buf, nbyte, written);
			memcpy(&FDS[fd].pipe->pb->data[FDS[fd].pipe->pb->length], buf, written);
			FDS[fd].pipe->pb->length += written;
			wait_target = FDS[fd].pipe;
		}
	} else if (FDS[fd].type == BOGFD_PIC1) {
		FDS[fd].status[1] = 1;
		written = 1;
		wait_target = &FDS[fd];
	} else {
		ERROR_PRINTF("write (%d, %08x, %d) = EBADF\n", fd, buf, nbyte);
		return -EBADF;
	}
	if (wait_target != NULL && wait_target->flags & BOGFD_BLOCKED_READ) {
		//TODO avoid thundering herd, maybe
		for (int thread = 0; thread < MAX_THREADS; ++thread) {
			if (threads[thread].state == THREAD_IO_READ && (struct BogusFD *) threads[thread].wait_target == wait_target) {
				threads[thread].state = THREAD_RUNNABLE;
			} else if (threads[thread].state == THREAD_POLL) {
				threads[thread].state = THREAD_RUNNABLE;
			}
		}
		FDS[fd].flags &= ~BOGFD_BLOCKED_READ;
	} else {
		for (int thread = 0; thread < MAX_THREADS; ++thread) {
			if (threads[thread].state == THREAD_POLL) {
				threads[thread].state = THREAD_RUNNABLE;
			}
		}
	}
	return written;
}

size_t kern_read(int fd, void * buf, size_t nbyte, int async) {
	if (fd < 0 || fd >= BOGFD_MAX) {
		return -EBADF;
	}
	while (1) {
		size_t read = 0;
		if (FDS[fd].type == BOGFD_PIPE) {
			if (FDS[fd].pb->length) {
				read = min(nbyte, FDS[fd].pb->length);
				memcpy(buf, FDS[fd].pb->data, read);
				FDS[fd].pb->length -= read;
				if (FDS[fd].pb->length) {
					memmove(&FDS[fd].pb->data, &FDS[fd].pb->data[read], FDS[fd].pb->length);
				}
			} else if (FDS[fd].pipe == NULL) {
				return 0;
			}
		} else if (FDS[fd].type == BOGFD_FILE) {
			DEBUG_PRINTF("read (%d, %p, %d)\n", fd, buf, nbyte);
			read = min(nbyte, FDS[fd].file->end - FDS[fd].pos);
			memcpy(buf, FDS[fd].pos, read);
			FDS[fd].pos += read;
			return read;
		} else {
			return -EBADF;
		}
		if (read) {
			return read;
		} else if (async) {
			return -EAGAIN;
		} else {
			FDS[fd].flags |= BOGFD_BLOCKED_READ;
			threads[current_thread].wait_target = (uintptr_t) &FDS[fd];
			switch_thread(THREAD_IO_READ, 0);
		}
	}
}

// separate function, because uint64_t breaks stack setup for thr_new otherwise
int kern_umtx_op(struct _umtx_op_args *a) {
	switch (a->op) {
		case UMTX_OP_WAKE: {
			uintptr_t phys = kern_mmap_physical((uintptr_t) a->obj);
			u_long count = a->val;
			for (int thread = 0; count > 0 &&  thread < MAX_THREADS; ++thread) {
				if (threads[thread].state == THREAD_UMTX_WAIT && threads[thread].wait_target == phys) {
					threads[thread].state = THREAD_RUNNABLE;
					--count;
				}
			}
			return 0;
		}
		case UMTX_OP_NWAKE_PRIVATE: {
			for (int i = 0; i < a->val; ++i) {
				uintptr_t phys = kern_mmap_physical(((uintptr_t *)a->obj)[i]);
				for (int thread = 0; thread < MAX_THREADS; ++thread) {
					if (threads[thread].state == THREAD_UMTX_WAIT && threads[thread].wait_target == phys) {
						threads[thread].state = THREAD_RUNNABLE;
					}
				}
			}
			return 0;
		}
		case UMTX_OP_WAIT_UINT:
		case UMTX_OP_WAIT_UINT_PRIVATE: {
			if (*(u_int *)a->obj == a->val) {
				uint64_t timeout = 0;
				if ((size_t) a->uaddr1 == sizeof(struct _umtx_time)) {
					struct _umtx_time *utime = (struct _umtx_time *)a->uaddr2;
					timeout = FIXED_POINT_TIME_NANOSECOND(utime->_timeout.tv_sec, utime->_timeout.tv_nsec);
					if (!(utime->_flags & UMTX_ABSTIME)) {
						timeout += fixed_point_time;
					}
				} else if ((ssize_t) a->uaddr1 == sizeof(struct timespec)) {
					halt("umtx wait_uint with timespec timeout\n");
				}
				threads[current_thread].wait_target = kern_mmap_physical((uintptr_t) a->obj);
				if (switch_thread(THREAD_UMTX_WAIT, timeout)) {
					return -ETIMEDOUT;
				}
			}
			return 0;
		}
		case UMTX_OP_MUTEX_WAIT: {
			struct umutex *mutex = a->obj;
			while (1) {
				// TODO locking/atomics
				if ((mutex->m_owner & ~UMUTEX_CONTESTED) == UMUTEX_UNOWNED) {
					break;
				}
				if (a->uaddr1 != NULL || a->uaddr2 != NULL) {
					halt("umtx mutex_wait with timeout\n");
				}
				mutex->m_owner |= UMUTEX_CONTESTED;
				threads[current_thread].wait_target = (uintptr_t) a->obj;
				switch_thread(THREAD_UMTX_MUTEX_WAIT, 0);
			}
			return 0;
		}
		case UMTX_OP_MUTEX_WAKE2: {
			struct umutex *mutex = a->obj;
			int found = 0;
			if ((mutex->m_owner & ~UMUTEX_CONTESTED) != UMUTEX_UNOWNED) {
				found = 1;
			}
			for (int thread = 0; thread < MAX_THREADS; ++thread) {
				if (threads[thread].state == THREAD_UMTX_MUTEX_WAIT &&
					threads[thread].wait_target == (uintptr_t) a->obj) {
					if (found == 0) {
						threads[thread].state = THREAD_RUNNABLE;
						found = 1;
					} else {
						mutex->m_owner |= UMUTEX_CONTESTED;
						break;
					}
				}
			}
			return 0;
		}
	}

	ERROR_PRINTF("_umtx_op(%08x, %d, %d, %08x, %08x)\n", a->obj, a->op, a->val, a->uaddr1, a->uaddr2);
	halt("unknown umtx op");
}

// separate function, because uint64_t breaks stack setup for thr_new otherwise
int kern_ppoll(struct ppoll_args *a) {
	uint64_t timeout = 0;
	if (a->ts != NULL) {
		timeout = fixed_point_time + FIXED_POINT_TIME_NANOSECOND(a->ts->tv_sec, a->ts->tv_nsec);
	}
	int printed = 0;
	int changedfds = 0;
	while (1) {
		/*if (!printed) { // && a->ts->tv_sec > 60) {
			printed = 1;
			ERROR_PRINTF("ppoll for %d fds, timeout %d.%d\n", a->nfds, FIXED_POINT_SECONDS(timeout), FIXED_POINT_MILLISECONDS(timeout));
			for (int i = 0; i < a->nfds; ++i) {
				ERROR_PRINTF("  FD %d: events %08x -> %08x\n", a->fds[i].fd, a->fds[i].events, a->fds[i].revents);
			}
		}*/
		for (int i = 0; i < a->nfds; ++i) {
			if (a->fds[i].fd < 0 || a->fds[i].fd >= BOGFD_MAX) { continue; } // no EBADF?
			struct BogusFD *fd = &FDS[a->fds[i].fd];
			a->fds[i].revents = 0;
			if (a->fds[i].events && fd->type == BOGFD_PIPE && fd->pipe == NULL) {
				a->fds[i].revents = a->fds[i].events;
				++changedfds;
				continue;
			}
			if ((a->fds[i].events & POLLIN && fd->type == BOGFD_PIC1 && fd->status[1] != 0) ||
			    (a->fds[i].events & POLLIN && fd->type == BOGFD_PIPE && fd->pb->length != 0)
			   ) {
				a->fds[i].revents |= POLLIN;
				++changedfds;
			}
			if (a->fds[i].events & POLLOUT && fd->type == BOGFD_PIPE && fd->pipe->pb->length < BOGFD_PB_LEN) {
				a->fds[i].revents |= POLLOUT;
				++changedfds;
			}
		}
		if (changedfds) {
			return changedfds;
		}
		if (switch_thread(THREAD_POLL, timeout)) {
			return 0;
		};
	}
}


#define CARRY 1
#define SYSCALL_SUCCESS(ret) { iframe->flags &= ~CARRY; return ret; }
#define SYSCALL_FAILURE(ret) { iframe->flags |= CARRY; return ret; }

int handle_syscall(uint32_t call, struct interrupt_frame *iframe)
{	
	void *argp = (void *)(iframe->sp + sizeof(iframe->sp));
	switch(call) {
		case SYS_read: {
			struct read_args *a = argp;
			ssize_t read = kern_read(a->fd, a->buf, a->nbyte, FDS[a->fd].flags & O_NONBLOCK);
			if (read < 0) {
				SYSCALL_FAILURE(-read);
			}
			SYSCALL_SUCCESS(read);
		}
		case SYS_readv: {
			struct readv_args *a = argp;
			struct iovec *iov = a->iovp;
			unsigned int iovcnt = a->iovcnt;
			ssize_t ret = 0;
			while (iovcnt) {
				ssize_t nbyte = kern_read(a->fd, iov->iov_base, iov->iov_len, iovcnt != a->iovcnt || FDS[a->fd].flags & O_NONBLOCK);
				if (nbyte > 0) {
					ret += nbyte;
				}
				if (nbyte != a->iovcnt) {
					if (nbyte >= 0) {
						SYSCALL_SUCCESS(ret);
					} else if (ret >= 0) {
						SYSCALL_SUCCESS(ret);
					} else {
						SYSCALL_FAILURE(-nbyte);
					}
				}
			}
			SYSCALL_SUCCESS(ret);
		}
		case SYS_pread: {
			struct pread_args *a = argp;
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("pread (...) = EBADF\n");
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == BOGFD_FILE) {
				DEBUG_PRINTF("pread (%d, %p, %d)\n", a->fd, a->buf, a->nbyte);
				size_t read = 0;
				struct BogusFD *fd = &FDS[a->fd];
				if (a->offset > 0 && a->offset < fd->file->size) {
					read = min(a->nbyte, fd->file->size - a->offset);
					memcpy(a->buf, fd->file->start + a->offset, read);
				}
				SYSCALL_SUCCESS(read);
			}
			ERROR_PRINTF("pread (%d, ...) = EBADF\n", a->fd);
			SYSCALL_FAILURE(EBADF);
		}
		case SYS_sendto: {
			struct sendto_args *a = argp;
			while (1) {
				ssize_t ret = write(a->s, a->buf, a->len);
				if (ret > 0) {
					SYSCALL_SUCCESS(ret);
				} else if (ret == 0) {
					if (FDS[a->s].flags & O_NONBLOCK) {
						SYSCALL_FAILURE(EAGAIN);
					} else {
						FDS[a->s].flags |= BOGFD_BLOCKED_WRITE;
						threads[current_thread].wait_target = (uintptr_t) &FDS[a->s];
						switch_thread(THREAD_IO_WRITE, 0);
					}
				} else {
					SYSCALL_FAILURE(-ret);
				}
			}
		}
		case SYS_write: {
			struct write_args *a = argp;
			while (1) { 
				ssize_t ret = write(a->fd, a->buf, a->nbyte);
				if (ret > 0) {
					SYSCALL_SUCCESS(ret);
				} else if (ret == 0) {
					if (FDS[a->fd].flags & O_NONBLOCK) {
						SYSCALL_FAILURE(EAGAIN);
					} else {
						FDS[a->fd].flags |= BOGFD_BLOCKED_WRITE;
						threads[current_thread].wait_target = (uintptr_t) &FDS[a->fd];
						switch_thread(THREAD_IO_WRITE, 0);
					}
				} else {
					SYSCALL_FAILURE(-ret);
				}
			}
		}
		case SYS_writev: {
			struct writev_args *a = argp;
			struct iovec *iov = a->iovp;
			unsigned int iovcnt = a->iovcnt;
			while (1) {
				ssize_t ret = 0;
				while (iovcnt) {
					ssize_t nbyte = write(a->fd, iov->iov_base, iov->iov_len);
					if (nbyte < 0) {
						SYSCALL_FAILURE(-ret);
					}
					ret += nbyte;
					if (nbyte != iov->iov_len) {
						break;
					}
					++iov;
					--iovcnt;
				}
				if (ret > 0) {
					SYSCALL_SUCCESS(ret);
				} else if (ret == 0) {
					if (FDS[a->fd].flags & O_NONBLOCK) {
						SYSCALL_FAILURE(EAGAIN);
					} else {
						FDS[a->fd].flags |= BOGFD_BLOCKED_WRITE;
						threads[current_thread].wait_target = (uintptr_t) &FDS[a->fd];
						switch_thread(THREAD_IO_WRITE, 0);
					}
				} else {
					ERROR_PRINTF("writev (%d, %08x, %d)\n", a->fd, a->iovp, a->iovcnt);
					SYSCALL_FAILURE(-ret);
				}
			}
		}
		case SYS_openat:
			argp += sizeof(argp);
		case SYS_open: {
			struct open_args *a = argp;
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != BOGFD_CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				ERROR_PRINTF("open (%s) = EMFILE\n", a->path);
				SYSCALL_FAILURE(EMFILE);
			}

			char path[256];
			strlcpy (path, a->path, sizeof(path));
			if (strcmp(path, ".") == 0) {
				strcpy(path, "");
			}

			struct hardcoded_file * file;
			if (a->flags & O_DIRECTORY) {
				size_t len = strlen(path);
				file = find_dir(path, len, 0);
				if (file) {
					FDS[next_fd].type = BOGFD_DIR;
					FDS[next_fd].file = file;
					FDS[next_fd].namelen = len;
					DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
					SYSCALL_SUCCESS(next_fd++);
				}
			} else {
				file = find_file(path);
				if (file != NULL) {
					FDS[next_fd].type = BOGFD_FILE;
					FDS[next_fd].file = file;
					FDS[next_fd].pos = file->start;
					DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
					SYSCALL_SUCCESS(next_fd++);
				}
			}
			if (strcmp("/dev/null", path) == 0) {
				FDS[next_fd].type = BOGFD_NULL;
				DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
				SYSCALL_SUCCESS(next_fd++);
			}
			DEBUG_PRINTF ("open (%s, %08x) = ENOENT\n", path, a->flags);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_close: {
			struct close_args *a = argp;
			if (FDS[a->fd].type == BOGFD_PIPE) {
				if (FDS[a->fd].pipe == &FDS[0]) {
					find_cursor();
					term_print("unpiping STDIN\n");
					term_printn(FDS[a->fd].pb->data, FDS[a->fd].pb->length);

					kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb, PAGE_SIZE);
					FDS[0].type = BOGFD_TERMIN;
					FDS[0].buffer = NULL;
					FDS[0].file = NULL;
				} else if (FDS[a->fd].pipe == &FDS[1]) {
					find_cursor();
					term_print("unpiping STDOUT\n");
					term_printn(FDS[a->fd].pb->data, FDS[a->fd].pb->length);

					kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb, PAGE_SIZE);
					FDS[1].type = BOGFD_TERMOUT;
					FDS[1].file = NULL;
					FDS[1].buffer = NULL;
				} else if (FDS[a->fd].pipe == &FDS[2]) {
					find_cursor();
					term_print("unpiping STDERR\n");
					term_printn(FDS[a->fd].pb->data, FDS[a->fd].pb->length);

					kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb, PAGE_SIZE);
					FDS[2].type = BOGFD_TERMOUT;
					FDS[2].file = NULL;
					FDS[2].buffer = NULL;
				} else {
					if (FDS[a->fd].pipe == NULL) {
						kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb & ~(PAGE_SIZE -1), PAGE_SIZE);
					} else {
						FDS[a->fd].pipe->pipe = NULL;
					}
				}
			}

			if (FDS[a->fd].type != BOGFD_CLOSED) {
				DEBUG_PRINTF("close (%d)\n", a->fd);
				FDS[a->fd].type = BOGFD_CLOSED;
				FDS[next_fd].flags = 0;
				FDS[a->fd].file = NULL;
				FDS[a->fd].buffer = NULL;
				if (a->fd < next_fd) {
					next_fd = a->fd;
				}
				SYSCALL_SUCCESS(0);
			} else {
				ERROR_PRINTF("close (%d) = EBADF\n", a->fd);
				SYSCALL_FAILURE(EBADF);
			}
		}
		case SYS_getpid: SYSCALL_SUCCESS(2);
		case SYS_recvfrom: {
			struct recvfrom_args *a = argp;
			if (a->s < 0 || a->s > BOGFD_MAX) {
				ERROR_PRINTF("recvfrom (%d, ...) = EBADF\n", a->s);
				SYSCALL_FAILURE(EBADF);
			}
			int read = 0;
			struct BogusFD *fd = &FDS[a->s];
			if (fd->type == BOGFD_PIC1) {
				if (fd->status[1]) {
					a->buf[0] = '!';
					read = 1;
					fd->status[1] = 0;
				}
			} else if (fd->type == BOGFD_PIPE) {
				if (fd->pb->length) {
					read = min(a->len, fd->pb->length);
					memcpy(a->buf, fd->pb->data, read);
					fd->pb->length -= read;
					if (fd->pb->length) {
						memmove(fd->pb->data, &fd->pb->data[read], fd->pb->length);
					}
				} else if (fd->pipe == NULL) {
					SYSCALL_SUCCESS(0);
				}
			} else {
				ERROR_PRINTF("recvfrom (%d, ...) = ENOTSOCK\n", a->s);
				SYSCALL_FAILURE(ENOTSOCK);
			}
			if (read) {
				if (a->from != NULL) {
					bzero(a->from, *a->fromlenaddr);
				}
				SYSCALL_SUCCESS(read);
			} else {
				if (fd->flags & O_NONBLOCK) {
					SYSCALL_FAILURE(EAGAIN);
				} else {
					halt("blocking recvfrom");
				}
			}
		}
		case SYS_ioctl: {
			struct ioctl_args *a = argp;
			DEBUG_PRINTF("ioctl (%d, %08lx, ...)\n", a->fd, a->com);
			int ret = -1;
			switch (a->com) {
				case TIOCGETA: {
					if (a->fd >= BOGFD_MAX || (FDS[a->fd].type != BOGFD_TERMIN && FDS[a->fd].type != BOGFD_TERMOUT )) {
						SYSCALL_FAILURE(ENOTTY);
					}
					struct termios *t = (struct termios *)a->data;
					t->c_iflag = 11010;
					t->c_oflag = 3;
					t->c_cflag = 19200;
					t->c_lflag = 1483;
					//t->c_cc = "\004\377\377\177\027\025\022\b\003\034\032\031\021\023\026\017\001\000\024\377";
					t->c_ispeed = 38400;
					t->c_ospeed = 38400;
					SYSCALL_SUCCESS(0);
				}
				case TIOCSETA: {
					SYSCALL_SUCCESS(0);
				}
				case TIOCGWINSZ: {
					if (a->fd >= BOGFD_MAX || (FDS[a->fd].type != BOGFD_TERMIN && FDS[a->fd].type != BOGFD_TERMOUT )) {
						SYSCALL_FAILURE(ENOTTY);
					}
					struct winsize *w = (struct winsize *)a->data;
					w->ws_row = 25;
					w->ws_col = 80;
					ret = 0;
					SYSCALL_SUCCESS(0);
				}
			}
			DEBUG_PRINTF("fd %d, parm_len %ld, cmd %ld, group %c\n", a->fd, IOCPARM_LEN(a->com), a->com & 0xff, (char) IOCGROUP(a->com));
			SYSCALL_FAILURE(ENOTTY);
		}
		case SYS_munmap: {
			struct munmap_args *a = argp;
			DEBUG_PRINTF("munmap (%08x, %d)\n",
				a->addr, a->len);
			kern_munmap(0, (uintptr_t) a->addr, a->len);
			SYSCALL_SUCCESS(0);
		}
		case SYS_socket: {
			struct socket_args *a = argp;
			DEBUG_PRINTF("socket (%d, %d, %d)\n", a->domain, a->type, a->protocol);
			if (a->domain == PF_UNIX) {
				while (next_fd < BOGFD_MAX && FDS[next_fd].type != BOGFD_CLOSED) {
					++next_fd;
				}
				if (next_fd >= BOGFD_MAX) {
					ERROR_PRINTF("socket (..) = EMFILE\n");
					SYSCALL_FAILURE(EMFILE);
				}
				FDS[next_fd].type = BOGFD_UNIX;
				SYSCALL_SUCCESS(next_fd++);
			}
			SYSCALL_FAILURE(EACCES);
		}
		case SYS_bind: {
			struct bind_args *a = argp;
			if (a->s < 0 || a->s > BOGFD_MAX) {
				ERROR_PRINTF("bind (%d, %08x, %d) = EBADF\n", a->s, a->name, a->namelen);
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->s].type == BOGFD_UNIX) {
				if (strncmp("/kern/pic1/", ((const struct sockaddr *)a->name)->sa_data, 11) == 0) {
					int irq = ((const struct sockaddr *)a->name)->sa_data[11] - '0';
					if (irq >= 0 && irq < 8) {
						DEBUG_PRINTF("pic1 irq %d requested\n", irq);

						IDT[0x20 + irq].offset_1 = ((uint32_t)&pic1_int + (irq * 5)) & 0xFFFF;
						IDT[0x20 + irq].offset_2 = ((uint32_t)&pic1_int + (irq * 5)) >> 16;

						FDS[a->s].type = BOGFD_PIC1;
						FDS[a->s].status[0] = irq;
						FDS[a->s].status[1] = 0;
						UNCLAIMED_PIC_INT = 0;
						SYSCALL_SUCCESS(0);
					}
				} else if (strncmp("/kern/fd/", ((const struct sockaddr *)a->name)->sa_data, 9) == 0) {
					int fd = ((const struct sockaddr *)a->name)->sa_data[9] - '0';
					if (fd >= 0 && fd <= 2) {
						ERROR_PRINTF("fd %d requested by %d\n", fd, a->s);
						if (FDS[fd].type == BOGFD_TERMIN || FDS[fd].type == BOGFD_TERMOUT) {
							FDS[a->s].type = BOGFD_PIPE;
							FDS[a->s].pipe = &FDS[fd];
							if (!kern_mmap((uintptr_t*)&FDS[a->s].pb, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, 0)) {
								halt ("couldn't allocate buffer for /kern/fd/");
							}
							FDS[a->s].pb->length = 0;
							
							FDS[fd].type = BOGFD_PIPE;
							FDS[fd].pipe = &FDS[a->s];
							FDS[fd].pb = (struct pipe_buffer *)((uintptr_t)FDS[a->s].pb + (PAGE_SIZE >> 1));
							FDS[a->s].pb->length = 0;

							SYSCALL_SUCCESS(0);
						}
					}
				}
				SYSCALL_FAILURE(EACCES);
			}
			SYSCALL_FAILURE(ENOTSOCK);
		}

		case SYS_mprotect: SYSCALL_SUCCESS(0); //ignore
		case SYS_madvise: SYSCALL_SUCCESS(0); //ignore
		case SYS_fcntl: {
			struct fcntl_args *a = argp;
			if (a->fd < 0 || a->fd > BOGFD_MAX) {
				ERROR_PRINTF("fcntl (%d, ...) = EBADF\n", a->fd);
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == BOGFD_CLOSED) {
				ERROR_PRINTF("fcntl (%d, ...) = EBADF\n", a->fd);
				SYSCALL_FAILURE(EBADF);
			}
			switch (a->cmd) {
				case F_GETFL:
					SYSCALL_SUCCESS(FDS[a->fd].flags &
						(O_NONBLOCK | O_APPEND | O_DIRECT | O_ASYNC));
				case F_SETFL: {
					if ((a->arg & (O_NONBLOCK | O_APPEND | O_DIRECT | O_ASYNC)) != a->arg) {
						halt("bad args to fcntl F_SETFL");
					}
					FDS[a->fd].flags = (FDS[a->fd].flags & (O_NONBLOCK | O_APPEND | O_DIRECT | O_ASYNC)) | a->arg;
					SYSCALL_SUCCESS(0);
				}
			}
		}
		case SYS_select: {
			struct select_args *a = argp;
			if (a->nd == 0 && a->tv == NULL) {
				switch_thread(THREAD_WAIT_FOREVER, 0);
			}
			ERROR_PRINTF("select(%d, %p, %p, %p, %p)\n", a->nd, a->in, a->ou, a->ex, a->tv);
			break;
		}
		case SYS_gettimeofday: {
			struct gettimeofday_args *a = argp;
			if (a->tp != NULL) {
				a->tp->tv_sec = FIXED_POINT_SECONDS(fixed_point_time);
				a->tp->tv_usec = 0; // FIXED_POINT_MILLISECONDS(fixed_point_time);
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
					threads[current_thread].tls_base = base;
					ugs_base.base_1 = base & 0xFFFF;
					base >>= 16;
					ugs_base.base_2 = base & 0xFF;
					base >>= 8;
					ugs_base.base_3 = base & 0xFF;
					SYSCALL_SUCCESS(0);
				}
			}
			break;
		}
		case SYS_getrlimit: {
			struct __getrlimit_args *a = argp;
			switch (a->which) {
				case RLIMIT_STACK:
					a->rlp->rlim_cur = USER_STACK_SIZE;
					a->rlp->rlim_max = USER_STACK_SIZE;
					break;
				case RLIMIT_NOFILE:
					a->rlp->rlim_cur = BOGFD_MAX;
					a->rlp->rlim_max = BOGFD_MAX;
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
		case SYS_clock_gettime: {
			struct clock_gettime_args *a = argp;
			a->tp->tv_sec = FIXED_POINT_SECONDS(fixed_point_time);
			a->tp->tv_nsec = 0; // FIXED_POINT_NANOSECONDS(fixed_point_time);
			SYSCALL_SUCCESS(0);
		}
		case SYS_issetugid: {
			DEBUG_PRINTF("issetugid()\n");
			SYSCALL_SUCCESS(0);
		}
		case SYS_sched_yield: {
			switch_thread(THREAD_RUNNABLE, 0);
			SYSCALL_SUCCESS(0);
		}
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
			*a->id = THREAD_ID_OFFSET + current_thread;
			SYSCALL_SUCCESS(0);
		}
		case SYS__umtx_op: {
			int ret = kern_umtx_op((struct _umtx_op_args *) argp);
			if (ret >= 0) {
				SYSCALL_SUCCESS(0);
			} else {
				SYSCALL_FAILURE(-ret);
			}
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
			if (kern_mmap(&ret_addr, a->addr, a->len, a->prot, a->flags)) {
				if (a->fd != -1 && a->fd < BOGFD_MAX && FDS[a->fd].type == BOGFD_FILE) {
					if (a->pos == 0 && a->addr != NULL) { // && a->len > PAGE_SIZE) {
						ERROR_PRINTF("add-symbol-file %s -o 0x%08x\n", FDS[a->fd].file->name, a->addr);
					}

					if (a->pos + a->len > FDS[a->fd].file->size) {
						size_t avail = FDS[a->fd].file->size - a->pos;
						memcpy((void *)ret_addr, FDS[a->fd].file->start + a->pos, avail);
						explicit_bzero((void*)(ret_addr + avail), a->len - avail);
					} else {
						memcpy((void *)ret_addr, FDS[a->fd].file->start + a->pos, a->len);
					}
				} else if (a->prot != PROT_NONE && (a->prot & PROT_FORCE) == 0) {
					explicit_bzero((void*)ret_addr, a->len);
				}
				SYSCALL_SUCCESS(ret_addr);
			} else {
				SYSCALL_FAILURE(ret_addr);
			}
		}
		case SYS_pipe2: {
			struct pipe2_args *a = argp;
			int pipe1, pipe2;
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != BOGFD_CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				ERROR_PRINTF("pipe2 (...) = EMFILE (1)\n");
				SYSCALL_FAILURE(EMFILE);
			}
			pipe1 = next_fd;
			++next_fd;
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != BOGFD_CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				next_fd = pipe1;
				ERROR_PRINTF("pipe2 (...) = EMFILE (2)\n");
				SYSCALL_FAILURE(EMFILE);
			}
			pipe2 = next_fd;
			++next_fd;

			DEBUG_PRINTF("pipe2 (%p, %08x)\n", a->fildes, a->flags);
			if (!kern_mmap((uintptr_t*)&FDS[pipe1].pb, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, 0)) {
				next_fd = pipe1;
				ERROR_PRINTF("pipe2 (...) = ENOMEM\n");
				SYSCALL_FAILURE(ENOMEM);
			}
			FDS[pipe1].type = BOGFD_PIPE;
			FDS[pipe2].type = BOGFD_PIPE;
			FDS[pipe1].pipe = &(FDS[pipe2]);
			FDS[pipe2].pipe = &(FDS[pipe1]);
			FDS[pipe1].pb->length = 0;
			FDS[pipe2].pb = (struct pipe_buffer *)((uintptr_t)FDS[pipe1].pb + (PAGE_SIZE >> 1));
			FDS[pipe2].pb->length = 0;
			a->fildes[0] = pipe1;
			a->fildes[1] = pipe2;
			ERROR_PRINTF("pipe2 -> %d <-> %d\n", pipe1, pipe2);
			SYSCALL_SUCCESS(0);
		}
		case SYS_ppoll: {
			int ret = kern_ppoll((struct ppoll_args *) argp);
			if (ret >= 0) {
				SYSCALL_SUCCESS(ret);
			} else {
				SYSCALL_FAILURE(ret);
			};
		}
		case SYS_fstat: {
			struct fstat_args *a = argp;
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("fstat () = EBADF\n");
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == BOGFD_TERMIN || FDS[a->fd].type == BOGFD_TERMOUT) {
				explicit_bzero(a->sb, sizeof(*a->sb));
				a->sb->st_mode = S_IWUSR | S_IRUSR | S_IFCHR;
				SYSCALL_SUCCESS(0);
			} else if (FDS[a->fd].type == BOGFD_FILE) {
				explicit_bzero(a->sb, sizeof(*a->sb));
				struct BogusFD * fd = &FDS[a->fd];
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
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("fstatfs () = EBADF\n");
				SYSCALL_FAILURE(EBADF);
			}
			if ((FDS[a->fd].type == BOGFD_DIR || FDS[a->fd].type == BOGFD_FILE)) {
				bzero(a->buf, sizeof(struct statfs));
				a->buf->f_version = STATFS_VERSION;
				strlcpy(a->buf->f_fstypename, "BogusFS", sizeof(a->buf->f_fstypename));
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("fstatfs (%d)\n", a->fd);
			SYSCALL_FAILURE(EBADF);
		}
		case SYS_fstatat: {
			struct fstatat_args *a = argp;
			struct hardcoded_file * file;
			file = find_file(a->path);
			if (file != NULL) {
				explicit_bzero(a->buf, sizeof(*a->buf));
				a->buf->st_dev = BOGFD_FILE;
				a->buf->st_ino = (ino_t) file;
				a->buf->st_nlink = 1;
				a->buf->st_size = file->size;
				a->buf->st_mode = S_IRUSR | S_IFREG;
				SYSCALL_SUCCESS(0);
			}
			file = find_dir(a->path, strlen(a->path), NULL);
			if (file != NULL) {
				explicit_bzero(a->buf, sizeof(*a->buf));
				a->buf->st_dev = BOGFD_DIR;
				a->buf->st_ino = (ino_t) file;
				a->buf->st_nlink = 1;
				a->buf->st_size = file->size;
				a->buf->st_mode = S_IRUSR | S_IFDIR;
				SYSCALL_SUCCESS(0);
			}
			DEBUG_PRINTF("stat (%s, %p) = ENOENT \n", a->path, a->buf);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_getdirentries: {
			struct getdirentries_args *a = argp;
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("getdirentries () = EBADF\n");
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == BOGFD_DIR) {
				struct dirent *b = (struct dirent*) a->buf;
				if (a->basep != NULL) {
					*a->basep = (off_t) FDS[a->fd].file;
				}
				if (FDS[a->fd].file == NULL) {
					SYSCALL_SUCCESS(0);
				}
				bzero(b, sizeof(*b));
				b->d_fileno = (ino_t) FDS[a->fd].file;
				b->d_reclen = sizeof(*b);
				char * start = FDS[a->fd].file->name + FDS[a->fd].namelen + 1;
				char * nextslash = strchr(start, '/');

				if (nextslash != NULL) {
					b->d_type = DT_DIR;
					b->d_namlen = nextslash - start;
					strlcpy(b->d_name, start, b->d_namlen + 1);
					struct hardcoded_file * file = FDS[a->fd].file;
					while (FDS[a->fd].file != NULL && strncmp(
							FDS[a->fd].file->name + FDS[a->fd].namelen + 1,
							file->name + FDS[a->fd].namelen + 1,
							b->d_namlen + 1) == 0) {
						file = FDS[a->fd].file;
						FDS[a->fd].file = find_dir(file->name, FDS[a->fd].namelen, file + 1);
					}
				} else {
					b->d_type = DT_REG;
					strlcpy(b->d_name, start, sizeof(b->d_name));
					b->d_namlen = strlen(b->d_name);
					FDS[a->fd].file = find_dir(FDS[a->fd].file->name, FDS[a->fd].namelen, FDS[a->fd].file + 1);
				}
				b->d_off = (off_t) FDS[a->fd].file;
				SYSCALL_SUCCESS(b->d_reclen);
			}
			ERROR_PRINTF("getdirentries (%d)\n", a->fd);
			SYSCALL_FAILURE(EBADF);
		}
		case SYS_thr_new: {
			struct thr_new_args *a = argp;
			uintptr_t stack_page;
			while (threads[next_thread].state != THREAD_EMPTY && next_thread < MAX_THREADS) {
				++next_thread;
			}
			if (next_thread >= MAX_THREADS) {
				ERROR_PRINTF("thr_new (...) = EPROCLIM\n");
				SYSCALL_FAILURE(EPROCLIM);
			}
			if (!kern_mmap(&stack_page, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, MAP_STACK)) {
				ERROR_PRINTF("thr_new (...) = ENOMEM\n");
				SYSCALL_FAILURE(ENOMEM);
			}
			explicit_bzero((void *)stack_page, PAGE_SIZE);
			size_t new_thread = next_thread;
			++next_thread;
			threads[new_thread].state = THREAD_INITING;
			threads[new_thread].kern_stack_top = stack_page + PAGE_SIZE;
			threads[new_thread].tls_base = (uintptr_t)a->param->tls_base;
			bzero(&threads[new_thread].savearea, sizeof(threads[new_thread].savearea));
			uintptr_t stack;
			asm volatile ( "mov %%esp, %0" : "=a"(stack) :);
			size_t stack_size = threads[current_thread].kern_stack_top - stack;
			memcpy((void *)(threads[new_thread].kern_stack_top - stack_size), (void *)stack, stack_size);

			//setup_new_stack returns on the current thread, and the new thread
			uintptr_t new_stack_cur = setup_new_stack(threads[new_thread].kern_stack_top - stack_size);
			if (new_stack_cur) { // thr_new returns on existing thread
				ERROR_PRINTF("thr_new return (%d) on old thread (%d)\n", new_thread, current_thread);
				threads[new_thread].kern_stack_cur = new_stack_cur;
				*a->param->child_tid = *a->param->parent_tid = THREAD_ID_OFFSET + new_thread;
				struct interrupt_frame * new_frame = (struct interrupt_frame *) (threads[new_thread].kern_stack_top - sizeof(struct interrupt_frame));
				new_frame->ip = (uint32_t) a->param->start_func;
				new_frame->sp = (uint32_t) a->param->stack_base + a->param->stack_size - sizeof(a->param->arg);
				*(void **)new_frame->sp = a->param->arg;
				new_frame->sp -= sizeof(a->param->arg); //skip a spot for the return address from the initial function
				new_frame->flags &= ~CARRY;
				threads[new_thread].state = THREAD_RUNNABLE;
			} else { // thr_new returns on new thread
				ERROR_PRINTF("thr_new return on new thread (%d)\n", current_thread);
				asm volatile ( "finit" :: ); // clear fpu/sse state
				return 0; // break convention on purpose
			}
			SYSCALL_SUCCESS(0);
		}
		case SYS_clock_getres: SYSCALL_FAILURE(EINVAL); // TODO clock stuff
	}
				
	if (call < SYS_MAXSYSCALL) {
		unsigned int *args = argp;
		ERROR_PRINTF("Got syscall %d (%s) (%08x, %08x) @%08x\n", call, syscallnames[call], args[0], args[1], args[-1]);
	} else {
		ERROR_PRINTF("got unknown syscall %d\n", call);
	}
	halt ("halting\n");
}

__attribute__ ((interrupt))
void handle_gp(struct interrupt_frame *frame, uint32_t error_code)
{
	if (frame) {
		ERROR_PRINTF("Got #GP (%u) IP: %08x at ", error_code, frame->ip);
	} else {
		ERROR_PRINTF("Got #GP, no stack frame\n");
	}
	halt(NULL);
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
	halt("page fault");
}

__attribute__ ((interrupt))
void handle_ud(struct interrupt_frame *frame)
{
	ERROR_PRINTF("Got #UD IP: %08x at ", frame->ip);
	halt(NULL);
}
void interrupt_setup()
{
	// remap primary PIC so that the interrupts don't conflict with Intel exceptions
	
	uint8_t mask = inb(PORT_PIC1 + 1); // read current mask from PIC
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
				halt(NULL);
			}
			if (phead->p_offset > lastoffset) {
				size_t count = phead->p_offset - lastoffset;
				ERROR_PRINTF("zeroing %d bytes from %08x to %08x\n", count, load_addr + lastoffset, load_addr + lastoffset + count);
				explicit_bzero((uint8_t *)(load_addr + lastoffset), count);
			}

			if (phead->p_filesz > phead->p_memsz) {
				ERROR_PRINTF("elf header %d has p_filesz > p_memsz; halting\n", i);
				halt(NULL);
			}
			uint8_t *src = start + phead->p_offset;
			uint8_t *dst = (void*) (load_addr - first_virtual +  phead->p_vaddr);
			if (src != dst) {
				ERROR_PRINTF("copying %d bytes from %08x to %08x\n", phead->p_filesz, src, dst);
				memcpy(dst, src, phead->p_filesz);
			}
			lastoffset = phead->p_offset + phead->p_filesz;
			uint32_t scratch;
			if (!kern_mmap(&scratch, (void *)(load_addr + phead->p_vaddr), phead->p_memsz, PROT_READ|PROT_WRITE|PROT_FORCE, 0)) {
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
	FDS[0].type = BOGFD_TERMIN;
	FDS[1].type = BOGFD_TERMOUT;
	FDS[2].type = BOGFD_TERMOUT;
	next_fd = 3;
	for (int i = next_fd; i < BOGFD_MAX; ++i) {
		FDS[i].type = BOGFD_CLOSED;
	}
}

void setup_entrypoint()
{
	struct hardcoded_file * file = find_file("/beam");
	if (!file) {
		ERROR_PRINTF("couldn't find /beam\n");
	}
	FDS[next_fd].type = BOGFD_FILE;
	FDS[next_fd].file = file;
	FDS[next_fd].pos = file->start;
	++next_fd;
	if (!kern_mmap(&user_stack, NULL, USER_STACK_SIZE, PROT_WRITE | PROT_READ, MAP_STACK | MAP_ANON)) {
		halt("couldn't get map for user stack\n");
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
	char *argv[] = {"/beam",
			"-S", "1:1", // one scheduler
			"-SDcpu", "1:1", //one dirty cpu scheduler
			"-SDio", "1", // one dirty i/o scheduler
			"--", "-root", "",
			"-progname", "erl", "--", "-home", "/",
			"-pz", "/obj/",
			"-s", "crazierl",
			"--", 
			NULL};

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
	threads[0].state = THREAD_RUNNING;

	threads[0].kern_stack_top = (uintptr_t) &stack_top;

	uintptr_t stack_page;
	if (!kern_mmap(&stack_page, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, MAP_STACK)) {
		halt("no stack page for idle thread");
	}
	explicit_bzero((void *)stack_page, PAGE_SIZE);
	threads[IDLE_THREAD].state = THREAD_IDLE;
	threads[IDLE_THREAD].kern_stack_top = stack_page + PAGE_SIZE;
	bzero(&threads[IDLE_THREAD].savearea, sizeof(threads[IDLE_THREAD].savearea));
	uintptr_t stack;
	asm volatile ( "mov %%esp, %0" : "=a"(stack) :);
	size_t stack_size = threads[current_thread].kern_stack_top - stack;
	memcpy((void *)(threads[IDLE_THREAD].kern_stack_top - stack_size), (void *)stack, stack_size);

	//setup_new_stack returns on the current thread, and the new thread
	uintptr_t new_stack_cur = setup_new_stack(threads[IDLE_THREAD].kern_stack_top - stack_size);
	if (new_stack_cur) { // thr_new returns on existing thread
		threads[IDLE_THREAD].kern_stack_cur = new_stack_cur;
	} else {
		asm volatile ( "finit" :: ); // clear fpu/sse state
		while (1) {
			asm volatile( "sti; hlt; cli" :: );
		}
	}

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

	get_time();
	
	kern_mmap_init(mb->mmap_length, mb->mmap_addr);
	
	uintptr_t scratch;
	ERROR_PRINTF("kernel read-only %08x - %08x\n", &__executable_start, &__etext);

	if (!kern_mmap(&scratch, &__executable_start, &__etext - &__executable_start, PROT_KERNEL | PROT_READ, 0)) {
		halt("couldn't map read only kernel section\n");
	}

	ERROR_PRINTF("kernel read-write %08x - %08x\n", &__data_start, &__edata);
	if (!kern_mmap(&scratch, &__data_start, &__edata - &__data_start, PROT_KERNEL | PROT_READ | PROT_WRITE, 0)) {
		halt("couldn't map read/write kernel section\n");
	}
	
	if (!kern_mmap(&scratch, (void *)vga_buffer, VGA_BUFFER_SIZE, PROT_KERNEL | PROT_FORCE | PROT_READ | PROT_WRITE, 0)) {
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
	halt("end of kernel!");
}
