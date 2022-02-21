#define USER_STACK_SIZE 1024 * 1024

#include "common.h"

#include <stddef.h>
#include <stdint.h>
#include <stdatomic.h>
#include <string.h>
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
#include <sys/timex.h>
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
#include <rtld_printf.h>
#include <x86intrin.h>
#include <cpuid.h>
#include <sys/socket.h>
#include <sys/thr.h>
#include <sys/event.h>
#include <x86/fpu.h>

#include "apic.h"
#include "files.h"
#include "kern_mmap.h"
#include "acpi.h"
#include "bogfd.h"

extern const char *syscallnames[];
extern void * handle_int_80;
extern void * ap_trampoline;
extern void * ap_trampoline2;
#define IRQ_STRIDE 8
#define FIRST_IRQ_VECTOR 0x20
#define TIMER_VECTOR FIRST_IRQ_VECTOR
#define SWITCH_VECTOR (FIRST_IRQ_VECTOR + 1)
#define HALT_VECTOR (FIRST_IRQ_VECTOR + 2)

extern void * gen_int;
extern void * gen_error;
extern void * stack_top;
extern void start_entrypoint();
extern uintptr_t setup_new_stack(uintptr_t new_stack_top, uintptr_t current_stack_top);
extern uintptr_t setup_new_idle(uintptr_t new_stack_top);
extern int switch_thread_impl(uintptr_t* oldstack, uintptr_t newstack);
extern _Noreturn void switch_ap_thread(uintptr_t newstack);
extern void __executable_start, __etext, __data_start, __edata, __tdata_start, __tdata_end, __locks_start, __locks_end;
extern uintptr_t tss_esp0;

char **environ = {NULL};
char *__progname = "crazierlkernel";

#define PORT_COM1 0x3f8   /* COM1 */

#define PIC_INTERRUPT_ACK 0x20





// First, let's do some basic checks to make sure we are using our x86-elf cross-compiler correctly
#if defined(__linux__)
	#error "This code must be compiled with a cross-compiler"
#elif !defined(__i386__)
	#error "This code must be compiled with an x86-elf compiler"
#endif



// include this last; use _KERNEL to avoid conflicting but unused definition
// of int sysarch between sysproto.h and sysarch.h

#define _KERNEL
#include <machine/sysarch.h>
#undef _KERNEL


DECLARE_LOCK(all_fds);
struct BogusFD FDS[BOGFD_MAX];
DECLARE_LOCK(all_bnotes);
struct bnote BNOTES[BNOTE_MAX];
size_t next_fd;
size_t next_bnote;
uint8_t next_irq_vector = FIRST_IRQ_VECTOR;
DECLARE_LOCK(all_irqs);
volatile int cpus_initing = 1;

uint8_t WANT_NMI = 0;
 
#define MAX_THREADS 128
#define THREAD_ID_OFFSET 100002
struct crazierl_thread threads[MAX_THREADS];
union savefpu savearea[MAX_THREADS];
struct threadqueue runqueue, timequeue, pollqueue;

__thread size_t current_thread = 0;
__thread size_t current_cpu = -1;
size_t next_thread = 0;
atomic_uint cpusonline = 1;

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

#define NUM_GDT (6 + (3 * MAX_CPUS))
#define GDT_GSBASE_OFFSET 6
#define GDT_TSS_OFFSET (GDT_GSBASE_OFFSET + (2 * MAX_CPUS))
struct GDTDescr GDT[NUM_GDT] =
	{ { // null GDT isn't read, so reuse it for GTD Descriptor
	    sizeof(GDT) * sizeof(GDT[0]) - 1
	  },
	  { }, // skip for alignment
	  { 0xFFFF,  // (0x10) user code, base 0, limit 0xFFFF FFFF
	    0, 0,
	    0xFA,    // Present, Ring 3, Normal, Executable, Non Conforming, Readable, Not accessed
	    0xCF,    // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
	    0 },
	  { 0xFFFF,  // (0x18) kernel code, base 0, limit 0xFFFF FFFF
	    0, 0,
	    0x9A,    // Present, Ring 0, Normal, Executable, Non Conforming, Readable, Not accessed
	    0xCF,    // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
	    0 },
	  { 0xFFFF,  // (0x20) user data, base 0, limit 0xFFFF FFFF
	    0, 0,
	    0xF2,    // Present, Ring 3, Normal, Data, Grows up, Writable, Not accessed
	    0xCF,    // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
	    0 },
	  { 0xFFFF,  // (0x28) kernel data, base 0, limit 0xFFFF FFFF
	    0, 0,
	    0x92,    // Present, Ring 0, Normal, Data, Grows up, Writable, Not accessed
	    0xCF,    // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
	    0 },
	};
// After this, element x is user GS base, element x + 1 is kernel GS base for each cpu
// Then, a TSS descriptor for each CPU too

struct TSS_Entry {
	uint32_t prev_tss; // unused for us
	uint32_t esp0;// kernel stack pointer
	uint32_t ss0; // kernel stack segment
	// we could have more things, but we're not going to
};

struct TSS_Entry TSS[MAX_CPUS];

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

volatile unsigned int TIMER_COUNT = 0;

DECLARE_LOCK(time_lock);
volatile uint64_t global_tsc_time;
volatile uint64_t global_time_offset;
volatile uint64_t global_tsc;
volatile int global_tsc_generation;
__thread uint64_t tsc_time;
__thread uint64_t time_offset;
__thread uint64_t tsc;
__thread int tsc_generation;
__thread uint64_t scaled_time_per_tick;

uintptr_t user_stack;

#define IOAPIC_PATH "/kern/ioapic/"
#define IRQ_PATH "/kern/irq/"

int check_bnote(size_t, struct BogusFD *);
void check_bnotes_fd(struct BogusFD *);
void summary();

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
//	outb(PORT_COM1 + 0, 0x80);    // Set divisor to 0x80 (lo byte) 300 baud
//	outb(PORT_COM1 + 1, 0x01);    //                0x01 (hi byte)
//	outb(PORT_COM1 + 0, 0x06);    // Set divisor to 0x06 (lo byte) 19200 baud
//	outb(PORT_COM1 + 1, 0x00);    //                0x00 (hi byte)
	outb(PORT_COM1 + 0, 0x0C);    // Set divisor to 0x0C (lo byte) 9600 baud
	outb(PORT_COM1 + 1, 0x00);    //                0x00 (hi byte)
	outb(PORT_COM1 + 0, 0x18);    // Set divisor to 0x0C (lo byte) 4800 baud
	outb(PORT_COM1 + 1, 0x00);    //                0x00 (hi byte)

	outb(PORT_COM1 + 3, 0x03);    // 8 bits, no parity, one stop bit
	outb(PORT_COM1 + 2, 0xC7);    // Enable FIFO, clear them, with 14-byte threshold
	outb(PORT_COM1 + 4, 0x0B);    // IRQs enabled, RTS/DSR set
}

int com_buffer_has_room() {
   return inb(PORT_COM1 + 5) & 0x20;
}

int com_buffer_idle() {
   return inb(PORT_COM1 + 5) & 0x40;
}

void write_serial(char a) {
	while (com_buffer_has_room() == 0) {
		_mm_pause();
	}
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
				//ERROR_PRINTF("unhandled control character %x\r\n", c);
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

void wake_cpu(size_t cpu) {
	local_apic_write(0x310, cpus[cpu].apic_id << 24);
	local_apic_write(0x300, 0x04000 | SWITCH_VECTOR);
}

_Noreturn
void halt(char * message, int mode) {
	find_cursor();
	if (message) {
		term_print(message);
	}
	for (int i = 2; i >= 0; --i) {
		LOCK(&FDS[i].lock);
		if (FDS[i].type == PIPE) {
			term_printn(FDS[i].pipe->pb->data, FDS[i].pipe->pb->length);
		}
		FDS[i].type = TERMOUT;
		UNLOCK(&FDS[i].lock);
	}
	if (mode != 1) {
		for (int cpu = 0; cpu < numcpu; ++cpu) {
			if (cpu != current_cpu) {
				local_apic_write(0x310, cpus[cpu].apic_id << 24);
				local_apic_write(0x300, 0x04000 | HALT_VECTOR);
			}
		}
	}
	atomic_fetch_add_explicit(&cpusonline, -1, memory_order_acq_rel);
	if (mode == 0 || mode == 2) {
		while (cpusonline != 0) { }
	}

	if (mode != 1) {
		summary();
	}

	if (mode == 2) {
		// reboot by forcing triple fault by causing interrupt after
		// clearing interrupt table

		IDTR.size = 0;
		asm volatile ( "lidt %0" :: "m" (IDTR) );
		wake_cpu(current_cpu);
		asm volatile ( "sti" :: );
	}
	while (1) {
		asm volatile ( "hlt" :: );
	}
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
	} while (bcmp(timeA, timeB, sizeof(timeB)) != 0);

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
	global_time_offset = FIXED_POINT_TIME_NANOSECOND(unix_time(timeA), 0);
	global_tsc_generation = 1;
}

DECLARE_LOCK(thread_st);


void wake_cpu_for_thread(size_t thread) {
	cpuset_t t_cpus;
	CPU_COPY(&threads[thread].cpus, &t_cpus);
	int cpu = CPU_FFS(&t_cpus) - 1;
	while (cpu != -1) {
		if (cpus[cpu].flags & CPU_IDLE) {
			cpus[cpu].flags &= ~CPU_IDLE; // don't wake multiple times!
			wake_cpu(cpu);
			break;
		}
		CPU_CLR(cpu, &t_cpus);
		cpu = CPU_FFS(&t_cpus) - 1;
	}
}

void mark_thread_runnable(size_t thread) {
	ASSERT_LOCK(&thread_st);
	if (threads[thread].state == RUNNABLE) {
		halt("remarking thread as runnable\r\n", 0);
	}
	if (threads[thread].timeout) {
		threads[thread].timeout = 0;
		if (threads[thread].flags & THREAD_PINNED) {
			unsigned int cpu = CPU_FFS(&threads[thread].cpus) - 1;
			TAILQ_REMOVE(&cpus[cpu].timequeue, &threads[thread], timeq);
		} else {
			TAILQ_REMOVE(&timequeue, &threads[thread], timeq);
		}
	}
	if (threads[thread].waitqhead) {
		TAILQ_REMOVE(threads[thread].waitqhead, &threads[thread], waitq);
		threads[thread].waitqhead = NULL;
	}
	
	if (threads[thread].flags & THREAD_PINNED) {
		unsigned int cpu = CPU_FFS(&threads[thread].cpus) - 1;
		TAILQ_INSERT_TAIL(&cpus[cpu].runqueue, &threads[thread], runq);
	} else {
		TAILQ_INSERT_TAIL(&runqueue, &threads[thread], runq);
	}
	threads[thread].state = RUNNABLE;
	wake_cpu_for_thread(thread);
}

// takes scaledppm to adjust time frequency; avoids duplicating time update function
uint64_t fixed_point_time(long scaledppm) {
	uint64_t fpt, tsc_count, tsc_increment;
	while (1) {
		if (global_tsc_generation != tsc_generation) {
			LOCK(&time_lock);
			tsc_time = global_tsc_time;
			time_offset = global_time_offset;
			tsc = global_tsc;
			tsc_generation = global_tsc_generation;
			scaled_time_per_tick = SCALED_S_PER_TSC_TICK;
			UNLOCK(&time_lock);
		}
		tsc_count = __rdtsc();
		tsc_increment = (tsc_count - tsc) * scaled_time_per_tick;
		fpt = (tsc_increment >> TSC_TICK_SCALE) + tsc_time;
		if (tsc_increment & (1LL << 63) || unlikely(scaledppm)) {
			LOCK(&time_lock);
			if (global_tsc_generation == tsc_generation) {
				if (scaledppm) {
					int64_t scale_adj = scaledppm;
					scale_adj *= scaled_time_per_tick;
					scale_adj /= 1000000;
					scale_adj >>= 16;
					scaled_time_per_tick += scale_adj;
					ERROR_PRINTF("time per tick %llu -> %llu\r\n", SCALED_S_PER_TSC_TICK, scaled_time_per_tick);
					SCALED_S_PER_TSC_TICK = scaled_time_per_tick;
				}

				tsc_time = global_tsc_time = fpt;
				tsc = global_tsc = tsc_count;
				tsc_generation = ++global_tsc_generation;
				UNLOCK(&time_lock);
				break;
			} else {
				UNLOCK(&time_lock);
			}
		} else {
			break;
		}
	}
	if (fpt > cpus[current_cpu].last_time) {
		cpus[current_cpu].last_time = fpt;
		return fpt;
	} else {
		ERROR_PRINTF("tsc_count %llu\r\ntsc_increment %llu\r\nfpt %llu\r\nlast_time%llu\r\n\r\n",
				tsc_count, tsc_increment, fpt, cpus[current_cpu].last_time);
		ERROR_PRINTF("tsc %llu\r\nglobal_tsc%llu\r\ntsc_time%llu\r\nglobal_tsc_time\r\n\r\n",
				tsc, global_tsc, tsc_time, global_tsc_time);
		ERROR_PRINTF("tsc generation %u\r\nglobal_tsc_generation%u\r\n\r\n",
				tsc_generation, global_tsc_generation);
		halt("would go back to the past\r\n", 0);
	}
}

int switch_thread(thread_state new_state, uint64_t timeout, int locked, struct threadqueue * waitq, struct lock * lock) {

	//DEBUG_PRINTF("current thread %d (%p), current cpu %d\r\n", current_thread, &current_thread, current_cpu);
	size_t old_thread = current_thread;
	size_t target = old_thread;
	if (threads[old_thread].state != RUNNING && threads[old_thread].state != IDLE) {
		halt("current thread isn't running or idle\r\n", 0);
	}
	int timed_out = 0;
	uint64_t current_time = fixed_point_time(0);
	uint64_t next_timeout = current_time + FIXED_POINT_TIME_NANOSECOND(300, 0);
	if (timeout && timeout <= current_time) {
		threads[target].wait_target = 0;
		if (locked) {
			UNLOCK(&thread_st);
		}
		if (lock) {
			UNLOCK(lock);
		}
		return 1; // don't switch if it's already past the time
	}
	if (!locked) {
		LOCK(&thread_st);
	} else {
		ASSERT_LOCK(&thread_st);
	}
	struct crazierl_thread *tp;
	tp = TAILQ_FIRST(&cpus[current_cpu].timequeue);
	if (tp != NULL && tp->timeout <= current_time) {
		TAILQ_REMOVE(&cpus[current_cpu].timequeue, tp, timeq);
		size_t i = tp - threads;
		target = i;
		timed_out = 1;
		threads[target].timeout = 0;
		threads[target].wait_target = 0;
		if (threads[target].waitqhead) {
			TAILQ_REMOVE(threads[target].waitqhead, tp, waitq);
			threads[target].waitqhead = NULL;
		}
		cpus[current_cpu].flags &= ~CPU_IDLE; // no longer idle
	} else { // no local timeout, check globals
		tp = TAILQ_FIRST(&timequeue);
		while (tp != NULL && tp->timeout <= current_time) {
			size_t i = tp - threads;
			if (CPU_ISSET(current_cpu, &tp->cpus)) {
				TAILQ_REMOVE(&timequeue, tp, timeq);
				target = i;
				timed_out = 1;
				threads[target].timeout = 0;
				threads[target].wait_target = 0;
				if (threads[target].waitqhead) {
					TAILQ_REMOVE(threads[target].waitqhead, tp, waitq);
					threads[target].waitqhead = NULL;
				}
				cpus[current_cpu].flags &= ~CPU_IDLE; // no longer idle
				break;
			} else {
				wake_cpu_for_thread(i);
				tp = TAILQ_NEXT(tp, timeq);
			}
		}
	}
	if (target == old_thread) { // no timeouts, check first item in global runqueue
		if (tp != NULL && tp->timeout < next_timeout) {
			next_timeout = tp->timeout;
		}
		tp = TAILQ_FIRST(&runqueue);
		if (tp != NULL && CPU_ISSET(current_cpu, &tp->cpus)) {
			TAILQ_REMOVE(&runqueue, tp, runq);
			size_t i = tp - threads;
			target = i;
			cpus[current_cpu].flags &= ~CPU_IDLE; // no longer idle
		}
	}
	if (target == old_thread) { // nothing from global runqueue, check pinned queue
		if (tp != NULL && tp->timeout < next_timeout) {
			next_timeout = tp->timeout;
		}
		tp = TAILQ_FIRST(&cpus[current_cpu].runqueue);
		if (tp != NULL) {
			TAILQ_REMOVE(&cpus[current_cpu].runqueue, tp, runq);
			size_t i = tp - threads;
			target = i;
			cpus[current_cpu].flags &= ~CPU_IDLE; // no longer idle
		}
	}
	if (target == old_thread) { // nothing from the runqueue
		cpus[current_cpu].flags |= CPU_IDLE;
		if (new_state == RUNNABLE) {
			UNLOCK(&thread_st);
			if (lock) {
				UNLOCK(lock);
			}
			arm_timer(next_timeout - current_time);
			return 0;
		} else {
			target = current_cpu + 1; // idle thread is cpu_number + 1, guaranteed by construction
		}
	}
	if (threads[target].waitqhead) {
		halt("shouldn't have waitqhead here\r\n", 0);
	}

	DEBUG_PRINTF("switching from %d (%d) to %d (%d) on cpu %d\r\n", old_thread, new_state, target, threads[target].state, current_cpu);
	DEBUG_PRINTF("new stack %p of %p\r\n",
		threads[target].kern_stack_cur, threads[target].kern_stack_top);
	DEBUG_PRINTF("old stack %p of %p\r\n",
		threads[old_thread].kern_stack_cur, threads[old_thread].kern_stack_top);
	uint64_t now = __rdtsc();
	threads[old_thread].time += now - threads[old_thread].start;
	threads[old_thread].start = 0;

	if (threads[old_thread].state != IDLE) {
		if (new_state == RUNNABLE) {
			mark_thread_runnable(old_thread);
		} else {
			threads[old_thread].state = new_state;
		}
	}
	if (timeout) {
		threads[old_thread].timeout = timeout;
		int found = 0;
		struct threadqueue * timehead = &timequeue;

		if (threads[old_thread].flags & THREAD_PINNED) {
			timehead = &cpus[current_cpu].timequeue;
		}
		TAILQ_FOREACH(tp, timehead, timeq) {
			if (timeout < tp->timeout) {
				found = 1;
				TAILQ_INSERT_BEFORE(tp, &threads[old_thread], timeq);
				break;
			}
		}
		if (!found) {
			TAILQ_INSERT_TAIL(timehead, &threads[old_thread], timeq);
		}
		if (timeout < next_timeout) {
			next_timeout = timeout;
		}
	}

	if (waitq) {
		threads[old_thread].waitqhead = waitq;
		TAILQ_INSERT_TAIL(threads[old_thread].waitqhead, &threads[old_thread], waitq);
	}
	if (lock) {
		UNLOCK(lock);
	}

	asm volatile ( "fxsave (%0)" :: "a"(&savearea[old_thread]) :);
	if (threads[target].start != 0) {
		halt("thread start time should be 0\r\n", 0);
	}
	threads[target].start = now;
	TSS[current_cpu].esp0 = threads[target].kern_stack_top;
	if (threads[target].state != IDLE) {
		threads[target].state = RUNNING;
		uintptr_t base = threads[target].tls_base;

		size_t user_gs = GDT_GSBASE_OFFSET + (current_cpu * 2) + 1;
		GDT[user_gs].base_1 = base & 0xFFFF;
		base >>= 16;
		GDT[user_gs].base_2 = base & 0xFF;
		base >>= 8;
		GDT[user_gs].base_3 = base & 0xFF;
		if (timed_out) {
			*(int *)threads[target].kern_stack_cur = 1;
		}
	}

	RELOCK(&thread_st, target);
	current_thread = target;
	if (threads[current_thread].state == IDLE) {
		arm_timer(next_timeout - current_time);
	} else {
		arm_timer(0);
	}
	cpus[current_cpu].current_thread = current_thread;
	int we_timed_out = switch_thread_impl(&threads[old_thread].kern_stack_cur, threads[target].kern_stack_cur);
	UNLOCK(&thread_st);
	asm volatile ( "fxrstor (%0)" :: "a"(&savearea[current_thread]) :);
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



int UNCLAIMED_IRQ = 0;
void handle_irq(unsigned int vector)
{
	local_apic_write(0xB0, 0); // EOI
	vector += FIRST_IRQ_VECTOR;
	uint64_t seed[3];
	seed[0] = vector;
	seed[1] = __rdtsc();
	seed[2] = fixed_point_time(0);
	rand_update(&seed, sizeof(seed));

	switch (vector) {
		case TIMER_VECTOR: {
			// TODO: If we want to schedule in 100ms slices, we can wait for 10 of these interrupts
			// or have a countdown timer
			if (unlikely(cpus_initing)) {
				if (current_cpu == 0) {
					++TIMER_COUNT;
				}
				arm_timer(0);
				break;
			}

			switch_thread(RUNNABLE, 0, 0, NULL, NULL);
			break;
		}
		case SWITCH_VECTOR: {
			switch_thread(RUNNABLE, 0, 0, NULL, NULL);
			break;
		}
		case HALT_VECTOR: {
			ERROR_PRINTF("halted by IPI, cpu %d, thread %d\r\n", current_cpu, current_thread);
			halt(NULL, 1);
		}
		default: {
			int found = 0;
			for (int fd = 0; fd < BOGFD_MAX; ++fd) {
				if (FDS[fd].type == IRQ && FDS[fd].status[0] == vector) {
					++found;
					write(fd, "!", 1);
				}
			}
			if (!found && !UNCLAIMED_IRQ) {
				UNCLAIMED_IRQ = 1;
				ERROR_PRINTF("unexpected interrupt vector %X on cpu %d\r\n", vector, current_cpu);
			}
		}
	}
}

void handle_unknown_error(struct interrupt_frame *frame, uint32_t irq, uint32_t error_code)
{
	ERROR_PRINTF("Got unexpected error %d (%d) IP: %08x cpu %d", irq, error_code, frame->ip, current_cpu);
	halt(NULL, 0);
}

ssize_t write(int fd, const void * buf, size_t nbyte) {
	struct BogusFD *wait_target = NULL;
	int written;
	if (fd < 0 || fd >= BOGFD_MAX) {
		return -EBADF;
	}
	LOCK(&FDS[fd].lock);
	if (FDS[fd].type == TERMOUT || FDS[fd].type == TERMIN) {
		term_printn(buf, nbyte);
		written = nbyte;
		UNLOCK(&FDS[fd].lock);
		return written; // bail out early, don't try to notify
	} else if (FDS[fd].type == PIPE) {
		if (FDS[fd].pipe == NULL) {
			UNLOCK(&FDS[fd].lock);
			return -EPIPE;
		} else {
			if (&FDS[fd] > FDS[fd].pipe) {
				// lock smallest FD first, so we have to relock
				UNLOCK(&FDS[fd].lock);
				LOCK(&FDS[fd].pipe->lock);
				LOCK(&FDS[fd].lock);
			} else {
				LOCK(&FDS[fd].pipe->lock);
			}

			written = min(nbyte, BOGFD_PB_LEN - FDS[fd].pipe->pb->length);
			//ERROR_PRINTF("write (%d, \"%s\", %d) = %d\r\n", fd, buf, nbyte, written);
			memcpy(&FDS[fd].pipe->pb->data[FDS[fd].pipe->pb->length], buf, written);
			FDS[fd].pipe->pb->length += written;
			wait_target = FDS[fd].pipe;
			check_bnotes_fd(FDS[fd].pipe);
			UNLOCK(&FDS[fd].pipe->lock);
		}
	} else if (FDS[fd].type == IRQ) {
		FDS[fd].status[1] = 1;
		written = 1;
		wait_target = &FDS[fd];
	} else {
		ERROR_PRINTF("write (%d, %08x, %d) = EBADF\r\n", fd, buf, nbyte);
		UNLOCK(&FDS[fd].lock);
		return -EBADF;
	}
	check_bnotes_fd(&FDS[fd]);
	UNLOCK(&FDS[fd].lock);
	LOCK(&thread_st);
	struct crazierl_thread *tp;
	if (wait_target != NULL && wait_target->flags & BOGFD_BLOCKED_READ) {
		while ((tp = TAILQ_FIRST(&wait_target->waiters)) != NULL) {
			size_t i = tp - threads;
			mark_thread_runnable(i);
		}
		wait_target->flags &= ~BOGFD_BLOCKED_READ;
	}
	while ((tp = TAILQ_FIRST(&pollqueue)) != NULL) {
		size_t i = tp - threads;
		mark_thread_runnable(i);
	}

	UNLOCK(&thread_st);
	return written;
}

size_t kern_read(int fd, void * buf, size_t nbyte, int force_async) {
	if (fd < 0 || fd >= BOGFD_MAX) {
		return -EBADF;
	}
	while (1) {
		LOCK(&FDS[fd].lock);
		size_t read = 0;
		if (FDS[fd].type == PIPE) {
			if (FDS[fd].pb->length) {
				read = min(nbyte, FDS[fd].pb->length);
				memcpy(buf, FDS[fd].pb->data, read);
				FDS[fd].pb->length -= read;
				if (FDS[fd].pb->length) {
					memmove(&FDS[fd].pb->data, &FDS[fd].pb->data[read], FDS[fd].pb->length);
				}
				FDS[fd].pb->data[FDS[fd].pb->length] = '\0'; // zero terminate to help with debugging
				if (FDS[fd].pipe != NULL) {
					if (&FDS[fd] > FDS[fd].pipe) {
						// lock smallest FD first, so we have to relock
						UNLOCK(&FDS[fd].lock);
						LOCK(&FDS[fd].pipe->lock);
						LOCK(&FDS[fd].lock);
					} else {
						LOCK(&FDS[fd].pipe->lock);
					}
					check_bnotes_fd(FDS[fd].pipe);
					UNLOCK(&FDS[fd].pipe->lock);
				}
			} else if (FDS[fd].pipe == NULL) {
				UNLOCK(&FDS[fd].lock);
				return 0;
			}
		} else if (FDS[fd].type == BOGFD_FILE) {
			DEBUG_PRINTF("read (%d, %p, %d)\r\n", fd, buf, nbyte);
			read = min(nbyte, FDS[fd].file->end - FDS[fd].pos);
			memcpy(buf, FDS[fd].pos, read);
			FDS[fd].pos += read;
			// return here, because we don't want to block on a file!
			UNLOCK(&FDS[fd].lock);
			return read;
		} else if (FDS[fd].type == IRQ) {
			if (FDS[fd].status[1]) {
				((char *)buf)[0] = '!';
				read = 1;
				FDS[fd].status[1] = 0;
			}
		} else {
			UNLOCK(&FDS[fd].lock);
			return -EBADF;
		}
		if (read) {
			check_bnotes_fd(&FDS[fd]);
			UNLOCK(&FDS[fd].lock);
			LOCK(&thread_st);
			struct crazierl_thread *tp;
			while ((tp = TAILQ_FIRST(&pollqueue)) != NULL) {
				size_t i = tp - threads;
				mark_thread_runnable(i);
			}
			UNLOCK(&thread_st);
			return read;
		} else if (force_async || FDS[fd].flags & O_NONBLOCK) {
			UNLOCK(&FDS[fd].lock);
			return -EAGAIN;
		} else {
			FDS[fd].flags |= BOGFD_BLOCKED_READ;
			switch_thread(IO_READ, 0, 0, &FDS[fd].waiters, &FDS[fd].lock);
		}
	}
}

void umtx_wake(void * obj, u_long count) {
	uintptr_t phys = kern_mmap_physical((uintptr_t) obj);
	LOCK(&thread_st);
	for (int thread = 0; count > 0 && thread < MAX_THREADS; ++thread) {
		if (threads[thread].state == UMTX_WAIT && threads[thread].wait_target == phys) {
			mark_thread_runnable(thread);
			--count;
		}
	}
	UNLOCK(&thread_st);
}

// separate function, because uint64_t breaks stack setup for thr_new otherwise
int kern_umtx_op(struct _umtx_op_args *a) {
	switch (a->op) {
		case UMTX_OP_WAKE: {
			umtx_wake(a->obj, a->val);
			return 0;
		}
		case UMTX_OP_NWAKE_PRIVATE: {
			for (int i = 0; i < a->val; ++i) {
				umtx_wake((((void **)a->obj)[i]), MAX_THREADS);
			}
			return 0;
		}
		case UMTX_OP_WAIT:
		case UMTX_OP_WAIT_UINT:
		case UMTX_OP_WAIT_UINT_PRIVATE: {
			LOCK(&thread_st);
			if ((a->op == UMTX_OP_WAIT && *(u_long *)a->obj == a->val) ||
			    (a->op != UMTX_OP_WAIT && *(u_int *)a->obj == a->val)) {
				uint64_t timeout = 0;
				if ((size_t) a->uaddr1 == sizeof(struct _umtx_time)) {
					struct _umtx_time *utime = (struct _umtx_time *)a->uaddr2;
					timeout = FIXED_POINT_TIME_NANOSECOND(utime->_timeout.tv_sec, utime->_timeout.tv_nsec);
					if (!(utime->_flags & UMTX_ABSTIME)) {
						timeout += fixed_point_time(0);
					}
				} else if ((ssize_t) a->uaddr1 == sizeof(struct timespec)) {
					halt("umtx wait_uint with timespec timeout\r\n", 0);
				}
				threads[current_thread].wait_target = kern_mmap_physical((uintptr_t) a->obj);
				if (switch_thread(UMTX_WAIT, timeout, 1, NULL, NULL)) {
					return -ETIMEDOUT;
				}
			} else {
				UNLOCK(&thread_st);
			}
			return 0;
		}
		case UMTX_OP_MUTEX_WAIT: {
			struct umutex *mutex = a->obj;
			if (a->uaddr1 != NULL || a->uaddr2 != NULL) {
				halt("umtx mutex_wait with timeout\r\n", 0);
			}
			LOCK(&thread_st);
			while (1) {
				uint32_t old = mutex->m_owner;
				if ((old & ~UMUTEX_CONTESTED) == UMUTEX_UNOWNED) {
					UNLOCK(&thread_st);
					break;
				}
				if (__sync_bool_compare_and_swap(&mutex->m_owner, old, old | UMUTEX_CONTESTED)) {
					threads[current_thread].wait_target = (uintptr_t) a->obj;
					switch_thread(UMTX_MUTEX_WAIT, 0, 1, NULL, NULL);
					LOCK(&thread_st);
				}
			}
			return 0;
		}
		case UMTX_OP_MUTEX_WAKE2: {
			struct umutex *mutex = a->obj;
			int found = 0;
			if ((mutex->m_owner & ~UMUTEX_CONTESTED) != UMUTEX_UNOWNED) {
				found = 1;
			}
			LOCK(&thread_st);
			for (int thread = 0; thread < MAX_THREADS; ++thread) {
				if (threads[thread].state == UMTX_MUTEX_WAIT &&
					threads[thread].wait_target == (uintptr_t) a->obj) {
					if (found == 0) {
						mark_thread_runnable(thread);
						found = 1;
					} else {
						while (1) {
							uint32_t old = mutex->m_owner;
							if (__sync_bool_compare_and_swap(&mutex->m_owner, old, old | UMUTEX_CONTESTED)) {
								break;
							}
						}
						break;
					}
				}
			}
			UNLOCK(&thread_st);
			return 0;
		}
	}

	ERROR_PRINTF("_umtx_op(%08x, %d, %d, %08x, %08x)\r\n", a->obj, a->op, a->val, a->uaddr1, a->uaddr2);
	halt("unknown umtx op\r\n", 0);
}

// separate function, because uint64_t breaks stack setup for thr_new otherwise
// TODO MAYBE\ rewrite to kqueue?
int kern_ppoll(struct ppoll_args *a) {
	//ERROR_PRINTF("ppoll nfds=%d!\r\n", a->nfds);
	uint64_t timeout = 0;
	if (a->ts != NULL) {
		timeout = fixed_point_time(0) + FIXED_POINT_TIME_NANOSECOND(a->ts->tv_sec, a->ts->tv_nsec);
	}
	int printed = 0;
	int changedfds = 0;
	while (1) {
		/*if (!printed) { // && a->ts->tv_sec > 60) {
			printed = 1;
			ERROR_PRINTF("ppoll for %d fds, timeout %d.%d\r\n", a->nfds, FIXED_POINT_SECONDS(timeout), FIXED_POINT_MILLISECONDS(timeout));
			for (int i = 0; i < a->nfds; ++i) {
				ERROR_PRINTF("  FD %d: events %08x -> %08x\r\n", a->fds[i].fd, a->fds[i].events, a->fds[i].revents);
			}
		}*/
		LOCK(&thread_st);
		for (int i = 0; i < a->nfds; ++i) {
			if (a->fds[i].fd < 0 || a->fds[i].fd >= BOGFD_MAX) { continue; } // no EBADF?
			struct BogusFD *fd = &FDS[a->fds[i].fd];
			a->fds[i].revents = 0;
			if (a->fds[i].events && fd->type == PIPE && fd->pipe == NULL) {
				a->fds[i].revents = a->fds[i].events;
				++changedfds;
				continue;
			}
			if ((a->fds[i].events & POLLIN && fd->type == IRQ && fd->status[1] != 0) ||
			    (a->fds[i].events & POLLIN && fd->type == PIPE && fd->pb->length != 0)
			   ) {
				a->fds[i].revents |= POLLIN;
				++changedfds;
			}
			if (a->fds[i].events & POLLOUT && fd->type == PIPE && fd->pipe->pb->length < BOGFD_PB_LEN) {
				a->fds[i].revents |= POLLOUT;
				++changedfds;
			}
		}
		if (changedfds) {
			UNLOCK(&thread_st);
			return changedfds;
		}
		if (switch_thread(POLL, timeout, 1, &pollqueue, NULL)) {
			return 0;
		};
	}
}

// assume kq and fd locked
size_t find_bnote(kq, fd, filter) {
	size_t i = FDS[fd].bnote;
	while (i < BNOTE_MAX) {
		if (BNOTES[i].kq == kq && BNOTES[i].filter == filter) {
			break;
		}
		i = BNOTES[i].selnext;
	}
	return i;
}

size_t new_bnote() {
	LOCK(&all_bnotes);
	while (next_bnote < BNOTE_MAX && BNOTES[next_bnote].filter != 0) {
		++next_bnote;
	}
	size_t ret = next_bnote;
	if (ret < BNOTE_MAX) {
		BNOTES[ret].filter = BNOTE_PENDING;
	}
	UNLOCK(&all_bnotes);
	return ret;
}

// assumed fd is locked
int check_bnote(size_t i, struct BogusFD * fd) {
	switch (BNOTES[i].filter) {
		case EVFILT_READ: {
			switch(fd->type) {
				case PIPE: {
					BNOTES[i].data = fd->pb->length;
					if (BNOTES[i].data > 0) {
						BNOTES[i].status = 1;
					} else {
						BNOTES[i].status = 0;
					}
					break;
				}
				case UNIX: { // not yet bound, can't read
					BNOTES[i].data = 0;
					BNOTES[i].status = 0;
					break;
				}
				case IRQ: {
					if (fd->status[1]) {
						BNOTES[i].data = 1;
						BNOTES[i].status = 1;
					} else {
						BNOTES[i].data = 0;
						BNOTES[i].status = 0;
					}
					break;
				}
				default:
					ERROR_PRINTF("fdtype %d\r\n", fd->type);
					halt("unexpected FD type in check_bnote EVFILT_READ\r\n", 0);
			}
			break;
		}
		case EVFILT_WRITE: {
			switch(fd->type) {
				case PIPE: {
					if (fd->pipe == NULL) {
						BNOTES[i].data = -1;
						BNOTES[i].status = -1;
						break;
					}
					BNOTES[i].data = BOGFD_PB_LEN - fd->pipe->pb->length;
					if (BNOTES[i].data > 0) {
						BNOTES[i].status = 1;
						break;
					} else {
						BNOTES[i].status = 0;
						break;
					}
				}
				case UNIX: { // not yet bound, can't write
					BNOTES[i].data = 0;
					BNOTES[i].status = 0;
					break;
				}
				case IRQ: { // can't write to an interrupt vector
					BNOTES[i].data = 0;
					BNOTES[i].status = 0;
					break;
				}
				default:
					ERROR_PRINTF("fdtype %d\r\n", fd->type);
					halt("unexpected FD type in check_bnote EVFILT_WRITE\r\n", 0);
			}
			break;
		}
		default:
			ERROR_PRINTF("filter %d\r\n", BNOTES[i].filter);
			halt("unexpected filter type in check_bnote\r\n", 0);
	}
	if (BNOTES[i].status != 0) {
		return !(BNOTES[i].flags & EV_DISABLE);
	}
	return 0;
}
void wake_kq(int kq) {
	if (kq != BOGFD_MAX) {
		LOCK(&thread_st);
		if (FDS[kq].flags & BOGFD_BLOCKED_READ) {
			struct crazierl_thread *tp;
			while ((tp = TAILQ_FIRST(&FDS[kq].waiters)) != NULL) {
				size_t i = tp - threads;
				mark_thread_runnable(i);
			}
			FDS[kq].flags &= ~BOGFD_BLOCKED_READ;
		}
		UNLOCK(&thread_st);
	}
}

void cleanup_bnotes(int fd, int is_kq) {
	int bn = FDS[fd].bnote;
	int next;
	int min_freed = BNOTE_MAX;

	while (bn != BNOTE_MAX) {
		LOCK(&BNOTES[bn].lock);
		BNOTES[bn].flags = EV_DROP;
		BNOTES[bn].filter = BNOTE_PENDING;
		BNOTES[bn].status = 0;
		if (is_kq) {
			next = BNOTES[bn].link;
			BNOTES[bn].link = BNOTE_MAX;
			BNOTES[bn].kq = BOGFD_MAX;
		} else {
			next = BNOTES[bn].selnext;
			BNOTES[bn].selnext = BNOTE_MAX;
			BNOTES[bn].fd = BOGFD_MAX;
		}
		if (BNOTES[bn].fd == BNOTES[bn].kq) {
			BNOTES[bn].flags = 0;
			BNOTES[bn].filter = 0;
			min_freed = min(min_freed, bn);
		}
		UNLOCK(&BNOTES[bn].lock);
		bn = next;
	}
}

void check_bnotes_fd(struct BogusFD * fd) {
	//TODO look for EV_DROP and cleanup
	size_t kq = BOGFD_MAX;
	for (int bnote = fd->bnote; bnote < BNOTE_MAX; bnote = BNOTES[bnote].selnext) {
		LOCK(&BNOTES[bnote].lock);
		if (check_bnote(bnote, fd) && BNOTES[bnote].kq != kq) {
			wake_kq(kq);
			kq = BNOTES[bnote].kq;
		}
		UNLOCK(&BNOTES[bnote].lock);
	}
	wake_kq(kq);
}

int kern_kevent(struct kevent_args *a) {
	size_t kq = a->fd;
	if (kq < 0 || kq >= BOGFD_MAX) {
		ERROR_PRINTF("kqueue (...) = EBADF\r\n");
		return -EBADF;
	}
	uint64_t timeout = 0;
	if (a->timeout != NULL) {
		timeout = fixed_point_time(0) + FIXED_POINT_TIME_NANOSECOND(a->timeout->tv_sec, a->timeout->tv_nsec);
		//ERROR_PRINTF("thread %d kqueue %d, %d changes, waiting for %d events, timeout %d.%06d\r\n", current_thread, kq, a->nchanges, a->nevents, a->timeout->tv_sec, a->timeout->tv_nsec);
	} else {
		//ERROR_PRINTF("thread %d kqueue %d, %d changes, waiting for %d events, timeout 0\r\n", current_thread, kq, a->nchanges, a->nevents);
	}
	LOCK(&FDS[kq].lock);
	if (FDS[kq].type != KQUEUE) {
		ERROR_PRINTF("kqueue (...) = EBADF\r\n");
		UNLOCK(&FDS[kq].lock);
		return -EBADF;
	}

	int nevents = 0;
	for (size_t i = 0; i < a->nchanges; ++i) {
		short filter = a->changelist[i].filter;
		if (filter == EVFILT_READ || filter == EVFILT_WRITE) {
			if (a->changelist[i].fflags) {
				halt("unexpected fflags in kevent", 0);
			}
			size_t fd = (size_t) a->changelist[i].ident;
			u_short flags = a->changelist[i].flags;
			LOCK(&FDS[fd].lock);
			if (FDS[fd].type == KQUEUE) {
				halt("can't kevent a kqueue, that's madness!", 0);
			}
			if (FDS[fd].type != CLOSED) {
				if (flags & (EV_ADD | EV_ENABLE | EV_DISABLE)) {
					if (flags & ~ (EV_ADD  | EV_ENABLE | EV_DISABLE | EV_DISPATCH)) {
						UNLOCK(&FDS[kq].lock);
						ERROR_PRINTF("unexpected flags 0x%x\r\n", flags);
						halt("halting\r\n", 0);
					}
					size_t x = find_bnote(kq, fd, filter);
					if (x == BNOTE_MAX && (flags & EV_ADD)) {
						x = new_bnote();
						if (x == BNOTE_MAX) {
							halt("kq ENOMEM", 0);
						}
						BNOTES[x].link = FDS[kq].bnote;
						FDS[kq].bnote = x;
						BNOTES[x].selnext = FDS[fd].bnote;
						FDS[fd].bnote = x;
						BNOTES[x].kq = kq;
						BNOTES[x].fd = fd;
					}
					if (x == BNOTE_MAX) {
						halt("kq ENOENT", 0);
					} else {
						// no need to lock bnote, because we hold fd and kq lock
						BNOTES[x].flags = flags;
						BNOTES[x].filter = filter;
						BNOTES[x].udata = a->changelist[i].udata;
						check_bnote(x, &FDS[fd]);
					}
				} else if (flags & EV_DELETE) {
					size_t x = find_bnote(kq, fd, filter);
					if (x != BNOTE_MAX) {
						BNOTES[x].flags = EV_DROP;
						BNOTES[x].filter = BNOTE_PENDING;
						BNOTES[x].status = 0;
					}
				} else {
					UNLOCK(&FDS[kq].lock);
					ERROR_PRINTF("flags 0x%x\r\n", flags);
					halt("flags", 0);
				}
				if (flags & EV_RECEIPT) {
					halt("kqueue EV_RECEIPT handling unimplemented", 0);
				}
			} else {
				halt("kevent EBADF", 0);
			}
			UNLOCK(&FDS[fd].lock);
		} else {
			UNLOCK(&FDS[kq].lock);
			ERROR_PRINTF("filter %d\r\n", filter);
			halt("filter", 0);
		}
	}
	if (a->nchanges) {
		wake_kq(kq); // in case there are other threades waiting on this kq, which can happen!
	}

	while (a->nevents) {
		size_t i = FDS[kq].bnote;
		size_t checked = 0;
		//TODO look for EV_DROP and cleanup
		while (nevents < a->nevents && i != BNOTE_MAX) {
			LOCK(&BNOTES[i].lock);
			if (BNOTES[i].status && !(BNOTES[i].flags & EV_DISABLE)) {
				//ERROR_PRINTF("got event fd %d, filter %d\r\n", BNOTES[i].fd, BNOTES[i].filter);
				EV_SET(&(a->eventlist[nevents]), BNOTES[i].fd, BNOTES[i].filter, BNOTES[i].flags,
				       BNOTES[i].fflags, BNOTES[i].data, BNOTES[i].udata);
				++nevents;
				if (BNOTES[i].flags & EV_DISPATCH) {
					BNOTES[i].flags = (BNOTES[i].flags & ~EV_DISPATCH) | EV_DISABLE;
				}
			}
			size_t nexti = BNOTES[i].link;
			UNLOCK(&BNOTES[i].lock);
			i = nexti;
			++checked;
		}

		if (nevents) {
			UNLOCK(&FDS[kq].lock);
			break;
		}
		//ERROR_PRINTF("thread %d kq %d sleeping until %lld (currently %lld, checked %d notes\r\n", current_thread, kq, timeout, fixed_point_time, checked);
		LOCK(&thread_st);
		FDS[kq].flags |= BOGFD_BLOCKED_READ;
		if (switch_thread(IO_READ, timeout, 1, &FDS[kq].waiters, &FDS[kq].lock)) {
			break;
		}
		LOCK(&FDS[kq].lock);
	}
	if (!a->nevents) {
		UNLOCK(&FDS[kq].lock);
	}
	//ERROR_PRINTF("thread %d leaving kqueue %d (%d events)\r\n", current_thread, kq, nevents);
	return nevents;
}


// separate function, because uint64_t breaks stack setup for thr_new otherwise
void kern_clock_gettimeofday (struct timeval *tp) {
	uint64_t time = fixed_point_time(0);
	time += time_offset;
	tp->tv_sec = FIXED_POINT_SECONDS(time);
	tp->tv_usec = FIXED_POINT_MICROSECONDS(time);
}

// separate function, because uint64_t breaks stack setup for thr_new otherwise
void kern_clock_settimeofday (struct timeval *tp) {
	while (1) {
		uint64_t time = fixed_point_time(0);
		LOCK(&time_lock);
		if (global_tsc_generation == tsc_generation) {
			global_time_offset = FIXED_POINT_TIME_NANOSECOND(tp->tv_sec, 1000 * tp->tv_usec) - time;
			++global_tsc_generation;
			UNLOCK(&time_lock);
			break;
		}
		UNLOCK(&time_lock);
	}
}
// separate function, because uint64_t breaks stack setup for thr_new otherwise
void kern_clock_gettime (clockid_t clock_id, struct timespec *tp) {
	uint64_t time = fixed_point_time(0);
	if (clock_id == CLOCK_REALTIME || clock_id == CLOCK_SECOND) {
		time += time_offset;
	}
	tp->tv_sec = FIXED_POINT_SECONDS(time);
	if (clock_id != CLOCK_SECOND) {
		tp->tv_nsec = FIXED_POINT_NANOSECONDS(time);
	}
}

#define CARRY 1
#define SYSCALL_SUCCESS(ret) { iframe->flags &= ~CARRY; return ret; }
#define SYSCALL_FAILURE(ret) { iframe->flags |= CARRY; return ret; }

int handle_syscall(uint32_t call, struct interrupt_frame *iframe)
{	
	void *argp = (void *)(iframe->sp + sizeof(iframe->sp));
	switch(call) {
	        case SYS_exit: {
			struct sys_exit_args *a = argp;
			ERROR_PRINTF("exit %d\r\n", a->rval);
			halt("rebooting\r\n", 2);
		}
		case SYS_fork: {
			SYSCALL_FAILURE(EAGAIN);
		}
		case SYS_read: {
			struct read_args *a = argp;
			ssize_t read = kern_read(a->fd, a->buf, a->nbyte, 0);
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
				ssize_t nbyte = kern_read(a->fd, iov->iov_base, iov->iov_len, iovcnt != a->iovcnt);
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
				ERROR_PRINTF("pread (...) = EBADF\r\n");
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == BOGFD_FILE) {
				DEBUG_PRINTF("pread (%d, %p, %d)\r\n", a->fd, a->buf, a->nbyte);
				size_t read = 0;
				struct BogusFD *fd = &FDS[a->fd];
				if (a->offset > 0 && a->offset < fd->file->size) {
					read = min(a->nbyte, fd->file->size - a->offset);
					memcpy(a->buf, fd->file->start + a->offset, read);
				}
				SYSCALL_SUCCESS(read);
			}
			ERROR_PRINTF("pread (%d, ...) = EBADF\r\n", a->fd);
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
						halt("bad locking, needs fixing\r\n", 0);
						FDS[a->s].flags |= BOGFD_BLOCKED_WRITE;
						switch_thread(IO_WRITE, 0, 0, &FDS[a->s].waiters, NULL);
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
						halt("bad locking needs fixing\r\n", 0);
						FDS[a->fd].flags |= BOGFD_BLOCKED_WRITE;
						switch_thread(IO_WRITE, 0, 0, &FDS[a->fd].waiters, NULL);
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
						halt("bad locking, needs fixing\r\n", 0);
						FDS[a->fd].flags |= BOGFD_BLOCKED_WRITE;
						switch_thread(IO_WRITE, 0, 0, &FDS[a->fd].waiters, NULL);
					}
				} else {
					ERROR_PRINTF("writev (%d, %08x, %d)\r\n", a->fd, a->iovp, a->iovcnt);
					SYSCALL_FAILURE(-ret);
				}
			}
		}
		case SYS_openat:
			argp += sizeof(argp);
		case SYS_open: {
			struct open_args *a = argp;
			LOCK(&all_fds);
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				ERROR_PRINTF("open (%s) = EMFILE\r\n", a->path);
				UNLOCK(&all_fds);
				SYSCALL_FAILURE(EMFILE);
			}
			size_t the_fd = next_fd;
			++next_fd;
			LOCK(&FDS[the_fd].lock);
			FDS[the_fd].type = PENDING;
			UNLOCK(&all_fds);

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
					FDS[the_fd].type = DIR;
					FDS[the_fd].file = file;
					FDS[the_fd].namelen = len;
					DEBUG_PRINTF("open (%s, ...) = %d\r\n", path, the_fd);
					UNLOCK(&FDS[the_fd].lock);
					SYSCALL_SUCCESS(the_fd);
				}
			} else {
				file = find_file(path);
				if (file != NULL) {
					FDS[the_fd].type = BOGFD_FILE;
					FDS[the_fd].file = file;
					FDS[the_fd].pos = file->start;
					DEBUG_PRINTF("open (%s, ...) = %d\r\n", path, the_fd);
					UNLOCK(&FDS[the_fd].lock);
					SYSCALL_SUCCESS(the_fd);
				}
			}
			if (strcmp("/dev/null", path) == 0) {
				FDS[the_fd].type = BOGFD_NULL;
				DEBUG_PRINTF("open (%s, ...) = %d\r\n", path, the_fd);
				UNLOCK(&FDS[the_fd].lock);
				SYSCALL_SUCCESS(the_fd);
			}
			DEBUG_PRINTF ("open (%s, %08x) = ENOENT\r\n", path, a->flags);
			LOCK(&all_fds);
			FDS[the_fd].type = CLOSED;
			UNLOCK(&FDS[the_fd].lock);
			if (the_fd < next_fd) {
				next_fd = the_fd;
			}
			UNLOCK(&all_fds);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_close: {
			struct close_args *a = argp;
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("close (...) = EBADF\r\n");
				SYSCALL_FAILURE(EBADF);
			}
			LOCK(&FDS[a->fd].lock);
			if (FDS[a->fd].type == PIPE) {
				if (FDS[a->fd].pipe == &FDS[0]) {
					find_cursor();
					term_print("unpiping STDIN\r\n");
					term_printn(FDS[a->fd].pb->data, FDS[a->fd].pb->length);

					kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb, PAGE_SIZE);
					FDS[0].type = TERMIN;
					FDS[0].buffer = NULL;
					FDS[0].file = NULL;
				} else if (FDS[a->fd].pipe == &FDS[1]) {
					find_cursor();
					term_print("unpiping STDOUT\r\n");
					term_printn(FDS[a->fd].pb->data, FDS[a->fd].pb->length);

					kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb, PAGE_SIZE);
					FDS[1].type = TERMOUT;
					FDS[1].file = NULL;
					FDS[1].buffer = NULL;
				} else if (FDS[a->fd].pipe == &FDS[2]) {
					find_cursor();
					term_print("unpiping STDERR\r\n");
					term_printn(FDS[a->fd].pb->data, FDS[a->fd].pb->length);

					kern_munmap(PROT_KERNEL, (uintptr_t) FDS[a->fd].pb, PAGE_SIZE);
					FDS[2].type = TERMOUT;
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

			cleanup_bnotes(a->fd, FDS[a->fd].type == KQUEUE);

			if (FDS[a->fd].type != CLOSED) {
				DEBUG_PRINTF("close (%d)\r\n", a->fd);
				FDS[a->fd].flags = 0;
				FDS[a->fd].file = NULL;
				FDS[a->fd].buffer = NULL;
				LOCK(&all_fds);
				FDS[a->fd].type = CLOSED;
				UNLOCK(&FDS[a->fd].lock);
				if (a->fd < next_fd) {
					next_fd = a->fd;
				}
				UNLOCK(&all_fds);
				SYSCALL_SUCCESS(0);
			} else {
				ERROR_PRINTF("close (%d) = EBADF\r\n", a->fd);
				UNLOCK(&FDS[a->fd].lock);
				SYSCALL_FAILURE(EBADF);
			}
		}
		case SYS_getpid: SYSCALL_SUCCESS(2);
		case SYS_geteuid: SYSCALL_SUCCESS(0);
		case SYS_recvfrom: {
			struct recvfrom_args *a = argp;
			int ret = kern_read(a->s, a->buf, a->len, 0);
			if (ret < 0) {
				SYSCALL_FAILURE(-ret);
			}
		        if (a->from != NULL) {
				bzero(a->from, *a->fromlenaddr);
			}
			SYSCALL_SUCCESS(ret);
		}
		case SYS_getsockname: {
			struct getsockname_args *a = argp;
			if (a->fdes < 0 || a->fdes >= BOGFD_MAX) {
				SYSCALL_FAILURE(EBADF);
			}
			LOCK(&FDS[a->fdes].lock);
			if (FDS[a->fdes].type != IRQ) {
				UNLOCK(&FDS[a->fdes].lock);
				SYSCALL_FAILURE(ENOTSOCK);
			}
			int len;
			//int len = rtld_snprintf(a->asa->sa_data, *a->alen, "%s%d", IRQ_PATH, FDS[a->fdes].status[0]);
			if (*a->alen < strlen(IRQ_PATH) + 3 + 1) { // max 256, plus 0
				halt("buffer is too small", 0);
			}

			len = rtld_snprintf(a->asa->sa_data, *a->alen, "%s%u", IRQ_PATH, FDS[a->fdes].status[0]);
			if (len + 1 > *a->alen) {
				halt("buffer was too small", 0);
			}
			a->asa->sa_family = AF_LOCAL;
			a->asa->sa_len = strlen(a->asa->sa_data) + 1;

			UNLOCK(&FDS[a->fdes].lock);
			SYSCALL_SUCCESS(0);
		}
		case SYS_access: {
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_ioctl: {
			struct ioctl_args *a = argp;
			DEBUG_PRINTF("ioctl (%d, %08lx, ...)\r\n", a->fd, a->com);
			int ret = -1;
			switch (a->com) {
				case TIOCGETA: {
					if (a->fd < 0 || a->fd >= BOGFD_MAX || (FDS[a->fd].type != TERMIN && FDS[a->fd].type != TERMOUT )) {
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
					if (a->fd < 0 || a->fd >= BOGFD_MAX || (FDS[a->fd].type != TERMIN && FDS[a->fd].type != TERMOUT )) {
						SYSCALL_FAILURE(ENOTTY);
					}
					struct winsize *w = (struct winsize *)a->data;
					w->ws_row = 25;
					w->ws_col = 80;
					ret = 0;
					SYSCALL_SUCCESS(0);
				}
			}
			DEBUG_PRINTF("fd %d, parm_len %ld, cmd %ld, group %c\r\n", a->fd, IOCPARM_LEN(a->com), a->com & 0xff, (char) IOCGROUP(a->com));
			SYSCALL_FAILURE(ENOTTY);
		}
		case SYS_readlink: {
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_munmap: {
			struct munmap_args *a = argp;
			DEBUG_PRINTF("munmap (%08x, %d)\r\n",
				a->addr, a->len);
			kern_munmap(0, (uintptr_t) a->addr, a->len);
			SYSCALL_SUCCESS(0);
		}
		case SYS_socket: {
			struct socket_args *a = argp;
			DEBUG_PRINTF("socket (%d, %d, %d)\r\n", a->domain, a->type, a->protocol);
			if (a->domain == PF_UNIX) {
				LOCK(&all_fds);
				while (next_fd < BOGFD_MAX && FDS[next_fd].type != CLOSED) {
					++next_fd;
				}
				if (next_fd >= BOGFD_MAX) {
					UNLOCK(&all_fds);
					ERROR_PRINTF("socket (..) = EMFILE\r\n");
					SYSCALL_FAILURE(EMFILE);
				}
				size_t the_fd = next_fd;
				LOCK(&FDS[the_fd].lock);
				FDS[the_fd].type = UNIX;
				++next_fd;
				UNLOCK(&all_fds);
				UNLOCK(&FDS[the_fd].lock);
				SYSCALL_SUCCESS(the_fd);
			}
			SYSCALL_FAILURE(EACCES);
		}
		case SYS_bind: {
			struct bind_args *a = argp;
			if (a->s < 0 || a->s > BOGFD_MAX) {
				ERROR_PRINTF("bind (%d, %08x, %d) = EBADF\r\n", a->s, a->name, a->namelen);
				SYSCALL_FAILURE(EBADF);
			}
			LOCK(&FDS[a->s].lock);
			if (FDS[a->s].type == UNIX) {
				char const* name = ((const struct sockaddr *)a->name)->sa_data;
				if (strncmp(IOAPIC_PATH, name, strlen(IOAPIC_PATH)) == 0) {
					char * endptr;
					long global_irq = strtol(name + strlen(IOAPIC_PATH), &endptr, 10);
					if (*endptr != '/') {
						halt("bad path for interrupt\r\n", 0);
					}
					long flags = strtol(endptr, NULL, 10);

					LOCK(&all_irqs);
					while (next_irq_vector < 0xF0) {
						if (!(IDT[next_irq_vector].type_attr & 0x80)) {
						    break;
						}
						++next_irq_vector;
					}
					if (next_irq_vector >= 0xF0) {
						halt("too many irq vectors were requested, max vector is 0xF0\r\n", 0);
					}
					uint8_t my_vector = next_irq_vector;
					++next_irq_vector;

					FDS[a->s].type = IRQ;
					FDS[a->s].status[0] = my_vector;
					FDS[a->s].status[1] = 0;
					FDS[a->s].status[2] = global_irq;

					IDT[my_vector].type_attr |= 0x80;
					UNLOCK(&all_irqs);
					
					ioapic_set_gsi_vector(global_irq, flags, my_vector, current_cpu);

					UNCLAIMED_IRQ = 0;
					check_bnotes_fd(&FDS[a->s]);
					UNLOCK(&FDS[a->s].lock);
					SYSCALL_SUCCESS(0);
				} else if (strncmp(IRQ_PATH, name, strlen(IRQ_PATH)) == 0) {
					char * endptr;
					long my_vector = strtol(name + strlen(IRQ_PATH), &endptr, 10);
					if (*endptr != '\0') {
						halt("bad path for interrupt\r\n", 0);
					}

					LOCK(&all_irqs);
					if (my_vector == 0) {
						while (next_irq_vector < 0xF0) {
							if (!(IDT[next_irq_vector].type_attr & 0x80)) {
							    break;
							}
							++next_irq_vector;
						}
						if (next_irq_vector >= 0xF0) {
							halt("too many irq vectors were requested, max vector is 0xF0\r\n", 0);
						}
						my_vector = next_irq_vector;
						++next_irq_vector;
					}

					FDS[a->s].type = IRQ;
					FDS[a->s].status[0] = my_vector;
					FDS[a->s].status[1] = 0;
					FDS[a->s].status[2] = 0;

					IDT[my_vector].type_attr |= 0x80;
					UNLOCK(&all_irqs);

					UNCLAIMED_IRQ = 0;
					check_bnotes_fd(&FDS[a->s]);
					UNLOCK(&FDS[a->s].lock);
					SYSCALL_SUCCESS(0);
				} else if (strncmp("/kern/fd/", name, 9) == 0) {
					int fd = name[9] - '0';
					if (fd >= 0 && fd <= 2) {
						ERROR_PRINTF("fd %d requested by %d\r\n", fd, a->s);
						LOCK(&FDS[fd].lock);
						if (FDS[fd].type == TERMIN || FDS[fd].type == TERMOUT) {
							FDS[a->s].type = PIPE;
							FDS[a->s].pipe = &FDS[fd];
							if (!kern_mmap((uintptr_t*)&FDS[a->s].pb, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, 0)) {
								halt ("couldn't allocate buffer for /kern/fd/", 0);
							}
							FDS[a->s].pb->length = 0;
							
							FDS[fd].type = PIPE;
							FDS[fd].pipe = &FDS[a->s];
							FDS[fd].pb = (struct pipe_buffer *)((uintptr_t)FDS[a->s].pb + (PAGE_SIZE >> 1));
							FDS[fd].pb->length = 0;
							check_bnotes_fd(&FDS[a->s]);
							check_bnotes_fd(&FDS[fd]);
							UNLOCK(&FDS[fd].lock);
							UNLOCK(&FDS[a->s].lock);
							SYSCALL_SUCCESS(0);
						}
						UNLOCK(&FDS[fd].lock);
					}
					UNLOCK(&FDS[a->s].lock);

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
				ERROR_PRINTF("fcntl (%d, ...) = EBADF\r\n", a->fd);
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == CLOSED) {
				ERROR_PRINTF("fcntl (%d, ...) = EBADF\r\n", a->fd);
				SYSCALL_FAILURE(EBADF);
			}
			switch (a->cmd) {
				case F_GETFL:
					SYSCALL_SUCCESS(FDS[a->fd].flags &
						(O_NONBLOCK | O_APPEND | O_DIRECT | O_ASYNC));
				case F_SETFL: {
					if ((a->arg & (O_NONBLOCK | O_APPEND | O_DIRECT | O_ASYNC)) != a->arg) {
						halt("bad args to fcntl F_SETFL", 0);
					}
					FDS[a->fd].flags = (FDS[a->fd].flags & (O_NONBLOCK | O_APPEND | O_DIRECT | O_ASYNC)) | a->arg;
					SYSCALL_SUCCESS(0);
				}
				case F_ISUNIONSTACK: {
					SYSCALL_SUCCESS(0);
				}
			}
		}
		case SYS_select: {
			struct select_args *a = argp;
			if (a->nd == 0 && a->tv == NULL) {
				ERROR_PRINTF("thread %d waiting forever\r\n", current_thread);
				switch_thread(WAIT_FOREVER, 0, 0, NULL, NULL);
			}
			ERROR_PRINTF("select(%d, %p, %p, %p, %p)\r\n", a->nd, a->in, a->ou, a->ex, a->tv);
			break;
		}
		case SYS_gettimeofday: {
			struct gettimeofday_args *a = argp;
			if (a->tp != NULL) {
				kern_clock_gettimeofday(a->tp);
			}
			if (a->tzp != NULL) {
				a->tzp->tz_minuteswest = 0;
				a->tzp->tz_dsttime = 0;
			}
			SYSCALL_SUCCESS(0);
		}
		case SYS_getsockopt: {
			SYSCALL_FAILURE(EBADF);
		}
		case SYS_settimeofday: {
			struct gettimeofday_args *a = argp;
			if (a->tzp != NULL) {
				if (a->tzp->tz_minuteswest != 0 || a->tzp->tz_dsttime != 0) {
					halt("timezones not supported\r\n", 0);
				}
			}
			if (a->tp != NULL) {
				kern_clock_settimeofday(a->tp);
			}
			SYSCALL_SUCCESS(0);
		}
		case SYS_sysarch: {
			struct sysarch_args *a = argp;
			switch (a->op) {
				case I386_SET_GSBASE: {
					uint32_t base = *((uint32_t *) a->parms);
					threads[current_thread].tls_base = base;
					size_t user_gs = GDT_GSBASE_OFFSET + (current_cpu * 2) + 1;
					GDT[user_gs].base_1 = base & 0xFFFF;
					base >>= 16;
					GDT[user_gs].base_2 = base & 0xFF;
					base >>= 8;
					GDT[user_gs].base_3 = base & 0xFF;
					SYSCALL_SUCCESS(0);
				}
			}
			break;
		}
		case SYS_ntp_adjtime: {
			struct ntp_adjtime_args *a = argp;
			unsigned int modes = a->tp->modes;
			if (modes & MOD_FREQUENCY) {
				modes &= ~MOD_FREQUENCY;
				fixed_point_time(a->tp->freq);
			}
			if (modes) {
				ERROR_PRINTF("unhandled modes in ntp_adjtime %X\r\n", modes);
				halt("halting\r\n", 0);
			}
			SYSCALL_SUCCESS(TIME_OK);
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
			DEBUG_PRINTF("setrlimit (%d, {%d, %d})\r\n", a->which, a->rlp->rlim_cur, a->rlp->rlim_max);
			switch (a->which) {
				case RLIMIT_STACK:
					if (a->rlp->rlim_cur > USER_STACK_SIZE) {
						SYSCALL_FAILURE(EPERM);
					}
					SYSCALL_SUCCESS(0);
			}
			break;
		}
		case SYS___sysctlbyname: { // probably need to check buffer addresses and lengths
			struct __sysctlbyname_args *a = argp;
			if (strncmp("kern.smp.cpus", a->name, a->namelen) == 0) {
				*(u_int *)a->old = numcpu;
				*a->oldlenp = sizeof(u_int);
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("sysctlbyname (\"%s\" ...)\r\n", a->name);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS___sysctl: { // probably need to check buffer addresses and lengths
			struct sysctl_args *a = argp;
			if (a->namelen == 2) {
				switch (a->name[0]) {
					case CTL_KERN: switch(a->name[1]) {
						case KERN_OSTYPE:
							strlcpy(a->old, "FreeBSD", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_OSRELEASE:
							strlcpy(a->old, "13.0-RELEASE", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_VERSION:
							strlcpy(a->old, "FreeBSD 13.0-RELEASE #0 releng/13.0-n244733-ea31abc261f: Fri Apr  9 04:24:09 UTC 2021\
    root@releng1.nyi.freebsd.org:/usr/obj/usr/src/amd64.amd64/sys/GENERIC", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_HOSTNAME:
							strlcpy(a->old, "node0.crazierl.org", *a->oldlenp);
							SYSCALL_SUCCESS(0);
						case KERN_ARND:{
							rand_bytes((uint8_t *)a->old, *a->oldlenp);
							SYSCALL_SUCCESS(0);
						}
						case KERN_OSRELDATE:
							*(u_int *)a->old = 1300139; // pretend to be freebsd 13.0 for now
							*a->oldlenp = sizeof(uint);
							SYSCALL_SUCCESS(0);
						case KERN_USRSTACK:
							*(uintptr_t *)a->old = user_stack;
							SYSCALL_SUCCESS(0);
						case KERN_IOV_MAX:
							*(uintptr_t *)a->old = IOV_MAX;
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
							DEBUG_PRINTF("hw.ncpu\r\n");
							*(u_int *)a->old = numcpu;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
						case HW_PAGESIZE:
							DEBUG_PRINTF("hw.pagesize\r\n");
							*(u_int *)a->old = PAGE_SIZE;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
					}
					case CTL_P1003_1B: switch (a->name[1]) {
						case CTL_P1003_1B_PAGESIZE:
							DEBUG_PRINTF("posix.pagesize\r\n");
							*(u_int *)a->old = PAGE_SIZE;
							*a->oldlenp = sizeof(u_int);
							SYSCALL_SUCCESS(0);
					}
				}
			}
			ERROR_PRINTF("__sysctl (%08x, %d, %08x, %d, %08x, %d)\r\n",
				a->name, a->namelen,
				a->old, *a->oldlenp,
				a->new, a->newlen);
			for (int i = 0; i < a->namelen; ++i) {
				ERROR_PRINTF("  %d\r\n", a->name[i]);
			}
			halt("sysctl\r\n", 0);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_clock_gettime: {
			struct clock_gettime_args *a = argp;
			switch(a->clock_id) {
				case CLOCK_MONOTONIC: /*fall through*/
				case CLOCK_UPTIME:
				case CLOCK_REALTIME:
				case CLOCK_SECOND:
					kern_clock_gettime(a->clock_id, a->tp);
					break;
				default:
					ERROR_PRINTF("clock gettime clock_id %d\r\n", a->clock_id);
					halt("need to handle other clock mode?", 0);
			}
			SYSCALL_SUCCESS(0);
		}
		case SYS_issetugid: {
			DEBUG_PRINTF("issetugid()\r\n");
			SYSCALL_SUCCESS(0);
		}
		case SYS___getcwd: {
			struct __getcwd_args *a = argp;
			if (a->buflen >= 2) {
				strlcpy(a->buf, "/", a->buflen);
				SYSCALL_SUCCESS(0);
			}
			SYSCALL_FAILURE(EINVAL);
		}
		case SYS_sched_yield: {
			switch_thread(RUNNABLE, 0, 0, NULL, NULL);
			SYSCALL_SUCCESS(0);
		}
		case SYS_sigprocmask: {
			struct sigprocmask_args *a = argp;
			DEBUG_PRINTF("sigprocmask (%d, %08x, %08x)\r\n", a->how, a->set, a->oset);
			SYSCALL_SUCCESS(0);
		}
		case SYS_sigaction: {
			struct sigaction_args *a = argp;
			DEBUG_PRINTF("sigaction (%d, %08x, %08x)\r\n", a->sig, a->act, a->oact);
			SYSCALL_SUCCESS(0);
		}
		case SYS_getcontext:
			ERROR_PRINTF("xxx sending back bogus success for getcontext\r\n");
			SYSCALL_SUCCESS(0);
		case SYS_thr_self: {
			struct thr_self_args *a = argp;
			DEBUG_PRINTF("thr_self()\r\n");
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
		case SYS_rtprio_thread: {
			struct rtprio_thread_args *a = argp;
			if (a->function == RTP_LOOKUP) {
				a->rtp->type = RTP_PRIO_NORMAL;
				SYSCALL_SUCCESS(0);
			}
			break;
		}
		case SYS_cpuset_getaffinity: {
			struct cpuset_getaffinity_args *a = argp;
			if (a->cpusetsize != sizeof(cpuset_t)) {
				ERROR_PRINTF("cpuset size %d, expecting %d\r\n", a->cpusetsize, sizeof(cpuset_t)) ;
				SYSCALL_FAILURE(ERANGE);
			}
			if (a->level == CPU_LEVEL_WHICH && a->which == CPU_WHICH_PID && a->id == -1) {
				CPU_ZERO(a->mask);
				for (int i = 0; i < numcpu; ++i) {
					CPU_SET(i, a->mask);
				}
				SYSCALL_SUCCESS(0);
			} else if (a->level == CPU_LEVEL_WHICH && a->which == CPU_WHICH_TID && a->id == -1) {
				CPU_COPY(&threads[current_thread].cpus, a->mask);
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("cpuset_getaffinity(%d, %d, %llx, %d, %08x)\r\n", a->level, a->which, a->id, a->cpusetsize, a->mask);
			break;
		}
		case SYS_cpuset_setaffinity: {
			struct cpuset_getaffinity_args *a = argp;
			if (a->cpusetsize != sizeof(cpuset_t)) {
				ERROR_PRINTF("cpuset size %d, expecting %d\r\n", a->cpusetsize, sizeof(cpuset_t)) ;
				SYSCALL_FAILURE(ERANGE);
			}
			if (a->level == CPU_LEVEL_WHICH && a->which == CPU_WHICH_TID && a->id == -1) {
				CPU_COPY(a->mask, &threads[current_thread].cpus);
				if (CPU_COUNT(&threads[current_thread].cpus) == 1) {
					threads[current_thread].flags |= THREAD_PINNED;
				} else {
					threads[current_thread].flags &= ~THREAD_PINNED;
				}
				if (!CPU_ISSET(current_cpu, &threads[current_thread].cpus)) {
					switch_thread(RUNNABLE, 0, 0, NULL, NULL);
				}
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("cpuset_setaffinity(%d, %d, %llx, %d, %08x)\r\n", a->level, a->which, a->id, a->cpusetsize, a->mask);
			break;
		}
		case SYS_mmap: {
			struct mmap_args *a = argp;
			uintptr_t ret_addr;

			if (kern_mmap(&ret_addr, a->addr, a->len, a->prot, a->flags)) {
				if (a->fd != -1 && a->fd < BOGFD_MAX && FDS[a->fd].type == BOGFD_FILE) {
					if (a->pos == 0 && a->addr != NULL) { // && a->len > PAGE_SIZE) {
						ERROR_PRINTF("add-symbol-file %s -o 0x%08x\r\n", FDS[a->fd].file->name, a->addr);
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
		case SYS_socketpair: {
		        struct socketpair_args *spa = argp;
			struct pipe2_args x;
			x.fildes = spa->rsv;
			x.flags = 0;
			argp = &x;
			/* fall-through */
		}
		case SYS_pipe2: {
			struct pipe2_args *a = argp;
			int pipe1, pipe2;
			LOCK(&all_fds);
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				ERROR_PRINTF("pipe2 (...) = EMFILE (1)\r\n");
				UNLOCK(&all_fds);
				SYSCALL_FAILURE(EMFILE);
			}
			pipe1 = next_fd;
			++next_fd;
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				next_fd = pipe1;
				ERROR_PRINTF("pipe2 (...) = EMFILE (2)\r\n");
				UNLOCK(&all_fds);
				SYSCALL_FAILURE(EMFILE);
			}
			pipe2 = next_fd;
			++next_fd;

			DEBUG_PRINTF("pipe2 (%p, %08x)\r\n", a->fildes, a->flags);
			if (!kern_mmap((uintptr_t*)&FDS[pipe1].pb, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, 0)) {
				next_fd = pipe1;
				ERROR_PRINTF("pipe2 (...) = ENOMEM\r\n");
				UNLOCK(&all_fds);
				SYSCALL_FAILURE(ENOMEM);
			}
			LOCK(&FDS[pipe1].lock);
			LOCK(&FDS[pipe2].lock);
			FDS[pipe1].type = PIPE;
			FDS[pipe2].type = PIPE;
			UNLOCK(&all_fds);
			FDS[pipe1].pipe = &(FDS[pipe2]);
			FDS[pipe2].pipe = &(FDS[pipe1]);
			FDS[pipe1].pb->length = 0;
			FDS[pipe2].pb = (struct pipe_buffer *)((uintptr_t)FDS[pipe1].pb + (PAGE_SIZE >> 1));
			FDS[pipe2].pb->length = 0;
			a->fildes[0] = pipe1;
			a->fildes[1] = pipe2;
			ERROR_PRINTF("pipe2 -> %d <-> %d\r\n", pipe1, pipe2);
			UNLOCK(&FDS[pipe1].lock);
			UNLOCK(&FDS[pipe2].lock);
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
				ERROR_PRINTF("fstat () = EBADF\r\n");
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == TERMIN || FDS[a->fd].type == TERMOUT) {
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
			DEBUG_PRINTF("fstat (%d)\r\n", a->fd);
			break;
		}
		case SYS_fstatfs: {
			struct fstatfs_args *a = argp;
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("fstatfs () = EBADF\r\n");
				SYSCALL_FAILURE(EBADF);
			}
			if ((FDS[a->fd].type == DIR || FDS[a->fd].type == BOGFD_FILE)) {
				bzero(a->buf, sizeof(struct statfs));
				a->buf->f_version = STATFS_VERSION;
				strlcpy(a->buf->f_fstypename, "BogusFS", sizeof(a->buf->f_fstypename));
				SYSCALL_SUCCESS(0);
			}
			ERROR_PRINTF("fstatfs (%d)\r\n", a->fd);
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
				a->buf->st_dev = DIR;
				a->buf->st_ino = (ino_t) file;
				a->buf->st_nlink = 1;
				a->buf->st_size = file->size;
				a->buf->st_mode = S_IRUSR | S_IFDIR;
				SYSCALL_SUCCESS(0);
			}
			DEBUG_PRINTF("stat (%s, %p) = ENOENT \r\n", a->path, a->buf);
			SYSCALL_FAILURE(ENOENT);
		}
		case SYS_getdirentries: {
			struct getdirentries_args *a = argp;
			if (a->fd < 0 || a->fd >= BOGFD_MAX) {
				ERROR_PRINTF("getdirentries () = EBADF\r\n");
				SYSCALL_FAILURE(EBADF);
			}
			if (FDS[a->fd].type == DIR) {
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
			ERROR_PRINTF("getdirentries (%d)\r\n", a->fd);
			SYSCALL_FAILURE(EBADF);
		}
		case SYS_thr_new: {
			struct thr_new_args *a = argp;
			uintptr_t stack_page;
			LOCK(&thread_st);

			while (threads[next_thread].state != EMPTY && next_thread < MAX_THREADS) {
				++next_thread;
			}
			size_t new_thread = next_thread;
			if (new_thread >= MAX_THREADS) {
				ERROR_PRINTF("thr_new (...) = EPROCLIM\r\n");
				SYSCALL_FAILURE(EPROCLIM);
			}
			if (threads[new_thread].kern_stack_top != 0) {
				stack_page = threads[new_thread].kern_stack_top - PAGE_SIZE;
			} else if (!kern_mmap(&stack_page, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, 0)) {
				ERROR_PRINTF("thr_new (...) = ENOMEM\r\n");
				SYSCALL_FAILURE(ENOMEM);
			}

			explicit_bzero((void *)stack_page, PAGE_SIZE);
			++next_thread;
			threads[new_thread].state = INITING;
			UNLOCK(&thread_st);
			CPU_COPY(&threads[current_thread].cpus, &threads[new_thread].cpus);
			threads[new_thread].flags = threads[current_thread].flags;
			threads[new_thread].kern_stack_top = stack_page + PAGE_SIZE;
			threads[new_thread].tls_base = (uintptr_t)a->param->tls_base;
			bzero(&savearea[new_thread], sizeof(savearea[new_thread]));

			uintptr_t new_stack_cur = setup_new_stack(threads[new_thread].kern_stack_top, threads[current_thread].kern_stack_top);
			DEBUG_PRINTF("thr_new return (%d) on old thread (%d) cpu %d\r\n", new_thread, current_thread, current_cpu);
			threads[new_thread].kern_stack_cur = new_stack_cur;
			*a->param->child_tid = *a->param->parent_tid = THREAD_ID_OFFSET + new_thread;
			struct interrupt_frame * new_frame = (struct interrupt_frame *) (threads[new_thread].kern_stack_top - sizeof(struct interrupt_frame));
			new_frame->ip = (uint32_t) a->param->start_func;
			new_frame->sp = (uint32_t) a->param->stack_base + a->param->stack_size - sizeof(a->param->arg);
			*(void **)new_frame->sp = a->param->arg;
			new_frame->sp -= sizeof(a->param->arg); //skip a spot for the return address from the initial function
			new_frame->flags &= ~CARRY;
			LOCK(&thread_st);
			mark_thread_runnable(new_thread);
			UNLOCK(&thread_st);
			SYSCALL_SUCCESS(0);
		}
		case SYS_thr_exit: {
			struct thr_exit_args *a = argp;
			if (a->state != NULL) {
				*(a->state) = 1;
			}
			umtx_wake(a->state, MAX_THREADS);
			LOCK(&thread_st);

			// TODO, check if there's any other non-empty thread
			if (current_thread < next_thread) {
				next_thread = current_thread;
			}
			switch_thread(EMPTY, 0, 1, NULL, NULL);
		}
		case SYS_clock_getres: SYSCALL_FAILURE(EINVAL); // TODO clock stuff
		case SYS_kqueue: {
			LOCK(&all_fds);
			while (next_fd < BOGFD_MAX && FDS[next_fd].type != CLOSED) {
				++next_fd;
			}
			if (next_fd >= BOGFD_MAX) {
				ERROR_PRINTF("kqueue (...) = EMFILE\r\n");
				UNLOCK(&all_fds);
				SYSCALL_FAILURE(EMFILE);
			}

			size_t the_fd = next_fd;
			++next_fd;
			LOCK(&FDS[the_fd].lock);
			FDS[the_fd].type = KQUEUE;
			UNLOCK(&all_fds);
			UNLOCK(&FDS[the_fd].lock);
			SYSCALL_SUCCESS(the_fd);
		}
		case SYS_kevent: {
			int ret = kern_kevent((struct kevent_args *) argp);
			if (ret >= 0) {
				SYSCALL_SUCCESS(ret);
			} else {
				SYSCALL_FAILURE(-ret);
			}
		}
		case SYS_thr_set_name: {
			struct thr_set_name_args *a = argp;
			rtld_snprintf(threads[a->id - THREAD_ID_OFFSET].name, sizeof(threads[0].name), "%s", a->name);
			SYSCALL_SUCCESS(0);
		}
		case SYS_getrandom: {
		        struct getrandom_args *a = argp;
		        rand_bytes(a->buf, a->buflen);
		        SYSCALL_SUCCESS(a->buflen);
		}
	}
				
	if (call < SYS_MAXSYSCALL) {
		unsigned int *args = argp;
		ERROR_PRINTF("Got syscall %d (%s) (%08x, %08x) @%08x\r\n", call, syscallnames[call], args[0], args[1], args[-1]);
	} else {
		ERROR_PRINTF("got unknown syscall %d\r\n", call);
	}
	halt ("halting\r\n", 0);
}

int thr_new_new_thread()
{
	UNLOCK(&thread_st);
	DEBUG_PRINTF("thr_new return on new thread (%d), cpu %d\r\n", current_thread, current_cpu);
	asm volatile ( "finit" :: ); // clear fpu/sse state
	return 0;
}



void handle_gp(struct interrupt_frame *frame, uint32_t error_code)
{
	if (frame) {
		ERROR_PRINTF("Got #GP (%u) IP: %08x cpu %d\r\n", error_code, frame->ip, current_cpu);
	} else {
		ERROR_PRINTF("Got #GP, no stack frame cpu %d\r\n", current_cpu);
	}
	halt(NULL, 0);
}

void handle_pf(struct interrupt_frame *frame, uint32_t error_code)
{
	uint32_t addr;
	asm volatile("mov %%cr2, %0" : "=a" (addr));

	if (frame) {
		ERROR_PRINTF("Got #PF (%u) address %08x, IP: %08x cpu %d, thread %d\r\n", error_code, addr, frame->ip, current_cpu, current_thread);
		kern_mmap_debug(addr);
	} else {
		ERROR_PRINTF("Got #PF, no stack frame, cpu %d\r\n", current_cpu);
	}
	halt("page fault\r\n", 0);
}

__attribute__ ((interrupt))
void handle_ud(struct interrupt_frame *frame)
{
	ERROR_PRINTF("Got #UD IP: %08x, cpu %d", frame->ip, current_cpu);
	halt(NULL, 0);
}

void handle_error(unsigned int vector, uint32_t error_code, struct interrupt_frame *frame)
{
	ERROR_PRINTF("error vector %d\r\n", vector);
	switch (vector) {
		case 0xd: handle_gp(frame, error_code); break;
		case 0xe: handle_pf(frame, error_code); break;
		default:
			ERROR_PRINTF("unknown error (0x%x): 0x%x, IP %08x cpu %d, thread %d\r\n", vector, error_code, frame->ip, current_cpu, current_thread);
	}
}

uint64_t readmsr (uint32_t msr)
{
	uint32_t hi, lo;
	uint64_t ret;
	asm volatile("rdmsr" : "=a"(lo), "=d"(hi) : "c"(msr));
	ret = hi;
	ret<<=32;
	ret |= lo;
	return ret;
}


void interrupt_setup()
{
	pic_setup(0xF0, 0xF0);

	char * rsdt = acpi_find_rsdt(NULL);
	if (rsdt == NULL) {
		halt("ACPI is required, but could not find RSDP", 1);
	}
	ERROR_PRINTF("RSDT is at %p\r\n", rsdt);
	if (! acpi_check_table(rsdt)) {
		halt("Invalid ACPI RSDT table\r\n", 1);
	}
	
	if (! acpi_process_madt(rsdt)) {
		halt("processing ACPI MADT (APIC) failed\r\n", 1);
	}
	
	uint64_t base_msr = readmsr(0x1b);
	
	if (!(base_msr & 0x800)) {
		halt("an enabled local APIC is required\r\n", 1);
	}
	if (local_apic != (base_msr & (~((1 << 12) - 1)))) {
		ERROR_PRINTF("local_apic from APIC %08x does not match value from MSR %08x\r\n", local_apic, base_msr & (~((1 << 12) - 1)));
		halt(NULL, 1);
	}

	// vector spurious intererrupts to 0xFF and enable APIC
	local_apic_write(0xF0, 0x1FF);
	
	ioapic_set_gsi_vector(timer_gsirq, timer_flags, TIMER_VECTOR, 0);

	// default to unknown_interrupt handler
	uint32_t handler;
	for (int i = FIRST_IRQ_VECTOR; i < (sizeof(IDT) / sizeof(IDT[0])); ++i) {
		handler = (uint32_t)(&gen_int);
		handler +=((i - FIRST_IRQ_VECTOR) * IRQ_STRIDE);
		IDT[i].offset_1 = handler & 0xFFFF;
		IDT[i].selector = 0x18;
		IDT[i].zero = 0;
		if (i >= 0xF0) { 
			IDT[i].type_attr = 0x8E;
		} else {
			IDT[i].type_attr = 0x0E;
		}
		IDT[i].offset_2 = handler >> 16;
	}

	IDT[0x06].selector = 0x18;
	IDT[0x06].type_attr = 0x8E;
	handler = (uint32_t)(&handle_ud);
	IDT[0x06].offset_1 = handler & 0xFFFF;
	IDT[0x06].offset_2 = handler >> 16;

	IDT[0x0D].selector = 0x18;
	IDT[0x0D].type_attr = 0x8E;
	handler = (uint32_t)(&gen_error);
	handler +=(0x0D * IRQ_STRIDE);
	IDT[0x0D].offset_1 = handler & 0xFFFF;
	IDT[0x0D].offset_2 = handler >> 16;

	IDT[0x0E].selector = 0x18;
	IDT[0x0E].type_attr = 0x8E;
	handler = (uint32_t)(&gen_error);
	handler +=(0x0E * IRQ_STRIDE);
	IDT[0x0E].offset_1 = handler & 0xFFFF;
	IDT[0x0E].offset_2 = handler >> 16;

	IDT[0x80].offset_1 = ((uint32_t) &handle_int_80) & 0xFFFF;
	IDT[0x80].offset_2 = ((uint32_t) &handle_int_80) >> 16;
	IDT[0x80].type_attr = 0xEE; // allow all rings to call in
	
	IDT[TIMER_VECTOR].type_attr |= 0x80;
	IDT[SWITCH_VECTOR].type_attr |= 0x80;
	IDT[HALT_VECTOR].type_attr |= 0x80;
	
	IDTR.size = sizeof(IDT) - 1;
	IDTR.offset = (uint32_t) &IDT;
	asm volatile ( "lidt %0" :: "m" (IDTR) );
	//ERROR_PRINTF("loaded idtl of size 0x%04x\r\n", IDTR.size);
}

void *entrypoint;
void *phead_start;
size_t phent, phnum; 
uintptr_t load_addr;

void load_file(void *start, char *name, size_t size)
{
	Elf32_Ehdr * head = (Elf32_Ehdr *) start;
	entrypoint = (void *) head->e_entry;
	DEBUG_PRINTF ("elf entrypoint 0x%08x\r\n", head->e_entry);
	phnum = head->e_phnum;
	phent = head->e_phentsize;
	Elf32_Phdr *phead = phead_start = start + head->e_phoff;

	DEBUG_PRINTF ("program binary size %d (0x%08x - 0x%08x)\r\n", size, start, start + size);
	DEBUG_PRINTF ("%d program headers of size %d at %08x\r\n", phnum, phent, phead_start);

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

	ERROR_PRINTF("load offsets %08x - %08x; virt %08x - %08x\r\n", first_addr, last_addr, first_virtual, last_virtual);
	ERROR_PRINTF("file space %08x; virt space %08x\r\n", last_addr - first_addr, last_virtual - first_virtual);
	uintptr_t virtual_space = last_virtual - first_virtual;
	uintptr_t space = max (last_addr - first_addr, last_virtual - first_virtual);
	load_addr = 0;
/*	if (0) { // kern_mmap_could_map(first_virtual, last_virtual)) {
	if (last_addr > size) {
		halt("load beyond file, halting\r\n", 1);
	}


	} else if (size >= last_addr) {
		DEBUG_PRINTF("using existing file memory to load\r\n");
		load_addr = (uintptr_t) start;
	} else { */
		if (!kern_mmap(&load_addr, 0, space, PROT_KERNEL | PROT_READ | PROT_WRITE, 0)){
			ERROR_PRINTF("couldn't map to load initial executable\r\n");
		}
		explicit_bzero((void *) load_addr, space);
//	}
	if (load_addr != first_virtual) {
		entrypoint = entrypoint - first_virtual + load_addr;
		ERROR_PRINTF("elf entrypoint moved to 0x%08x\r\n", entrypoint);
	}

	ERROR_PRINTF("add-symbol-file %s -o 0x%08x\r\n", name, load_addr);

	phead = phead_start;
	size_t last_vaddr = 0;
	for (int i = 0; i < head->e_phnum; ++i) {
		if (phead->p_type == PT_LOAD) {
			DEBUG_PRINTF( "  %d: PT_LOAD offset %08x, virt %08x, filesize 0x%08x, memsize 0x%08x\r\n",
				i, phead->p_offset, phead->p_vaddr,
				phead->p_filesz, phead->p_memsz);
			DEBUG_PRINTF( "      load address %08x - %08x\r\n", phead->p_vaddr + (load_addr - first_virtual), phead->p_memsz + phead->p_vaddr + (load_addr - first_virtual));
			if (phead->p_vaddr < last_vaddr) {
				ERROR_PRINTF("elf header %d has p_vaddr < last_vaddr; halting\r\n", i);
				halt(NULL, 1);
			}
			if (phead->p_vaddr > last_vaddr) {
				size_t count = phead->p_vaddr - last_vaddr;
				ERROR_PRINTF("zeroing %d bytes from %08x to %08x\r\n", count, load_addr + last_vaddr, load_addr + last_vaddr + count);
				explicit_bzero((uint8_t *)(load_addr + last_vaddr), count);
			}

			if (phead->p_filesz > phead->p_memsz) {
				ERROR_PRINTF("elf header %d has p_filesz > p_memsz; halting\r\n", i);
				halt(NULL, 1);
			}
			uint8_t *src = start + phead->p_offset;
			uint8_t *dst = (void*) (load_addr - first_virtual +  phead->p_vaddr);
			if (src != dst) {
				ERROR_PRINTF("copying %d bytes from %08x to %08x\r\n", phead->p_filesz, src, dst);
				memcpy(dst, src, phead->p_filesz);
			}
			last_vaddr = phead->p_vaddr + phead->p_filesz;
			uint32_t scratch;
			// TODO match permissions to load flags
			if (!kern_mmap(&scratch, (void *)(load_addr + phead->p_vaddr), phead->p_memsz + ((load_addr + phead->p_vaddr) & (PAGE_SIZE -1)), PROT_READ|PROT_WRITE|PROT_FORCE, 0)) {
				ERROR_PRINTF("couldn't map ELF load section %08x\r\n", load_addr + phead->p_vaddr);
			}
		}
		++phead;
	}
	if (virtual_space & (PAGE_SIZE - 1)) {
		virtual_space = (virtual_space & ~(PAGE_SIZE - 1)) + PAGE_SIZE;
	}
	size_t count = virtual_space - last_vaddr;
	if (count) {
		ERROR_PRINTF("zeroing final %d bytes from %08x to %08x\r\n", count, load_addr + last_vaddr, load_addr + last_vaddr + count);
		explicit_bzero((uint8_t*) (load_addr + last_vaddr), count);
	}
	kern_munmap(PROT_KERNEL, load_addr, virtual_space);
}

void enable_sse() {
	uint32_t a;
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
	for (int i = 0; i < BOGFD_MAX; ++i) {
		FDS[i].type = CLOSED;
		FDS[i].bnote = BNOTE_MAX;
		TAILQ_INIT(&FDS[i].waiters);
	}
	FDS[0].type = TERMIN;
	FDS[1].type = TERMOUT;
	FDS[2].type = TERMOUT;
	next_fd = 3;
	next_bnote = 0;
	for (int i = 0; i < BNOTE_MAX; ++i) {
		BNOTES[i].kq = BNOTES[i].fd = BOGFD_MAX;
		BNOTES[i].link = BNOTES[i].selnext = BNOTE_MAX;
	}
}

// some values should be 16-byte aligned on x86 for sse (i think)
#define MAX_ALIGN (16 - 1)

void start_cpus();

void idle() {
	asm volatile ( "finit" :: ); // clear fpu/sse state
	UNLOCK(&thread_st);
	start_cpus();
	while (1) {
		asm volatile( "sti; hlt; cli" :: );
	}
}

void setup_cpus()
{
	if (numcpu >= MAX_THREADS) {
		ERROR_PRINTF("MAX_THREADS set too low, minimum is numcpu (%d) + 1; 2 * numcpu would be better\r\n", numcpu);
		halt("too many CPUs", 1);
	}

	ERROR_PRINTF("kernel TLS %08x - %08x\r\n", &__tdata_start, &__tdata_end);
	// add a pointer to the end, and align both start and end
	size_t raw_tls_size = &__tdata_end - &__tdata_start;
	size_t tls_start_padding = (uintptr_t)&__tdata_start & MAX_ALIGN;
	size_t tls_end_padding = (((uintptr_t)&__tdata_end + sizeof(uintptr_t) + MAX_ALIGN) & ~MAX_ALIGN) - (uintptr_t)&__tdata_end;
	size_t padded_tls_size = raw_tls_size + tls_start_padding + tls_end_padding;
	ERROR_PRINTF("TLS size %d, padded %d\r\n", raw_tls_size, padded_tls_size);

	uintptr_t kernel_tls;
	if (!kern_mmap(&kernel_tls, NULL, numcpu * padded_tls_size, PROT_READ | PROT_WRITE | PROT_KERNEL, MAP_STACK)) {
		halt("couldn't allocate Kernel TLS memory\r\n", 1);
	}
	explicit_bzero((void *)kernel_tls, (numcpu * padded_tls_size + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1));
	ERROR_PRINTF("kernel_tls at %p\r\n", kernel_tls);

	strncpy(threads[0].name, "main", sizeof(threads[0].name));
	threads[0].state = RUNNING;
	CPU_ZERO(&threads[0].cpus);
	threads[0].kern_stack_top = (uintptr_t) &stack_top;
	unsigned int my_apic_id = local_apic_read(0x20) >> 24;
	//ERROR_PRINTF("my apic id %x\r\n", my_apic_id);
	size_t this_cpu = -1;

	for (int i = 0; i < numcpu; ++i) {
		cpus[i].current_thread = i + 1; // idle thread
		if (cpus[i].apic_id == my_apic_id) {
			this_cpu = i;
			CPU_SET(i, &threads[0].cpus);
			cpus[i].current_thread = 0;
		}
		// thread 0 starts as runnable on all CPUs
		CPU_SET(i, &threads[0].cpus);

		// setup cpu specific idle thread
		uintptr_t stack_page;
		if (!kern_mmap(&stack_page, NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_KERNEL, MAP_STACK)) {
			halt("no stack page for idle thread", 1);
		}
		explicit_bzero((void *)stack_page, PAGE_SIZE);
		CPU_ZERO(&threads[i + 1].cpus);
		CPU_SET(i, &threads[i + 1].cpus);
		threads[i + 1].state = IDLE;
		threads[i + 1].kern_stack_top = stack_page + PAGE_SIZE;
		bzero(&savearea[i + 1], sizeof(savearea[i + 1]));
		uintptr_t stack;
		asm volatile ( "mov %%esp, %0" : "=a"(stack) :);
		size_t stack_size = threads[0].kern_stack_top - stack;
		uintptr_t new_stack_cur = setup_new_idle(threads[i + 1].kern_stack_top);
		threads[i + 1].kern_stack_cur = new_stack_cur;
		threads[i + 1].state = IDLE;
		threads[i + 1].flags = THREAD_PINNED;
		rtld_snprintf(threads[i + 1].name, sizeof(threads[i + 1].name), "cpu %02x idle", i);
		size_t gsbase = GDT_GSBASE_OFFSET + (i * 2);

		// setup GS BASE descriptor for kernel
		// copy master TLS
		kernel_tls += tls_start_padding;
		memcpy((void *) kernel_tls, &__tdata_start, raw_tls_size);
		kernel_tls += raw_tls_size;
		*(uintptr_t *)kernel_tls = kernel_tls;
		GDT[gsbase].limit_1 = 0xFFFF;
		GDT[gsbase].base_1 = kernel_tls & 0xFFFF;
		GDT[gsbase].base_2 = (kernel_tls >> 16) & 0xFF;
		GDT[gsbase].access = 0x92; // Present, Ring 0, Normal, Data, Grows up, Writable, Not accessed
		GDT[gsbase].limit_2 = 0xCF; // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
		GDT[gsbase].base_3 = kernel_tls >> 24;
		kernel_tls += tls_end_padding;

		++gsbase;
		// setup GS BASE descriptor for user
		GDT[gsbase].limit_1 = 0xFFFF;
		GDT[gsbase].base_1 = 0;
		GDT[gsbase].base_2 = 0;
		GDT[gsbase].access = 0xF2; // Present, Ring 3, Normal, Data, Grows up, Writable, Not accessed
		GDT[gsbase].limit_2 = 0xCF; // 4K blocks, 32-bit selector, 2x reserved; limit 16:19
		GDT[gsbase].base_3 = 0;

		// Setup task state segments
		size_t tss = GDT_TSS_OFFSET + i;
		GDT[tss].limit_1 = sizeof(TSS[0]);
		GDT[tss].base_1 = ((uintptr_t)&TSS[i]) & 0xFFFF;
		GDT[tss].base_2 = ((uintptr_t)&TSS[i] >> 16) & 0xFF;
		GDT[tss].access = 0x89;
		GDT[tss].limit_2 = 0x40;
		GDT[tss].base_3 = (uintptr_t)&TSS[i] >> 24;

		TSS[i].esp0 = (uintptr_t)&stack_top;
		TSS[i].ss0 = 0x28;

		TAILQ_INIT(&cpus[i].runqueue);
		TAILQ_INIT(&cpus[i].timequeue);
	}
	uint16_t active_tss = ((GDT_TSS_OFFSET + this_cpu) * sizeof(GDT[0])) | 0x3;
	asm volatile ("ltr %0" :: "a"(active_tss));
	asm volatile ("mov %0, %%gs" :: "a"((GDT_GSBASE_OFFSET + 2 * this_cpu)* sizeof(GDT[0])));
	current_cpu = this_cpu;
	if (current_cpu == -1) {
		halt("could not find boot processor in cpu list?\r\n", 1);
	}
}

void setup_entrypoint()
{
	struct hardcoded_file * file = find_file("/beam");
	if (!file) {
		ERROR_PRINTF("couldn't find /beam\r\n");
	}
	FDS[next_fd].type = BOGFD_FILE;
	FDS[next_fd].file = file;
	FDS[next_fd].pos = file->start;
	++next_fd;
	if (!kern_mmap(&user_stack, NULL, USER_STACK_SIZE, PROT_WRITE | PROT_READ, MAP_STACK | MAP_ANON)) {
		halt("couldn't get map for user stack\r\n", 0);
	}
	//ERROR_PRINTF("%p\r\n", user_stack);
	user_stack += USER_STACK_SIZE; // we actually want to keep track of the top of the stack

	void* new_top = (void*) user_stack;

	// list of page sizes
	new_top -= sizeof(size_t);
	*(size_t *)new_top = 0;
	new_top -= sizeof(size_t);
	*(size_t *)new_top = 0x1000;
	void * page_sizes = new_top;

	// set up environment
	char * env[] = {"BINDIR=/", "ERL_INETRC=/cfg/inetrc",
		"TERM=vt100",
		//"LD_32_DEBUG=1",
		NULL};
	// set up arguments
	char topology[100];
	rtld_snprintf(topology, sizeof(topology), "L0-%dc0-%d", numcpu, numcpu);
	char *argv[] = {"/beam",
			"-sct", topology,
			"-sbt", "ns",
			"--", "-root", "",
			"-progname", "erl", "--", "-home", "/",
			"-pz", "/obj/",
			"-s", "crazierl",
			"-sbwt", "none",
			"-sbwtdcpu", "none",
			"-sbwtdio", "none",
			"-kernel", "inet_backend", "socket",
			"-proto_dist", "gen_tcp",
			"-epmd_module", "crazierl_epmd",
			"-kernel", "inet_dist_listen_min", "4370",
			"-no_epmd",
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

	if (cpus_initing) {
		ERROR_PRINTF ("waiting for cpus to init\r\n");
		while (cpus_initing) {
			asm volatile ( "hlt" :: );
		}
	}

	DEBUG_PRINTF ("jumping to %08x\r\n", entrypoint);
	threads[0].start = __rdtsc();
	start_entrypoint(new_top, entrypoint, ((GDT_GSBASE_OFFSET + 1)* sizeof(GDT[0])) | 0x3);
}

void init_cpus() {
	for (int i = 0; i < numcpu; ++i) {
		if (current_cpu == i) {
			cpus[i].flags |= CPU_STARTED;
		} else if ((cpus[i].flags & (CPU_ENABLED|CPU_STARTED)) == CPU_ENABLED) {
			//ERROR_PRINTF("trying to INIT cpu %d\r\n", i);
			local_apic_write(0x310, cpus[i].apic_id << 24);
			local_apic_write(0x300, 0x04500);
		} else {
			halt("numcpus probably shouldn't include disabled cpus??\r\n", 0);
		}
	}
	if (current_cpu == -1) {
		halt("could not find boot processor in cpu list?\r\n", 0);
	}
}

int start_cpu(size_t cpu, uint8_t page) {
	uint32_t command_word = (0x04600 | page);
	//ERROR_PRINTF("trying to START cpu %d: %x\r\n", cpu, command_word);
	local_apic_write(0x310, cpus[cpu].apic_id << 24);
	local_apic_write(0x300, command_word);

	unsigned int max_wait = TIMER_COUNT + 2;
	while (TIMER_COUNT < max_wait) {
		if (cpus[cpu].flags & CPU_STARTED) {
			return 1;
		}
	}
	ERROR_PRINTF("trying to START cpu %d a second time\r\n", cpu);
	local_apic_write(0x300, command_word);
	max_wait = TIMER_COUNT + 10;
	while (TIMER_COUNT < max_wait) {
		if (cpus[cpu].flags & CPU_STARTED) {
			return 1;
		}
	}
	ERROR_PRINTF("couldn't start cpu %d\r\n", cpu);
	cpus[cpu].flags &= ~CPU_ENABLED;
	return 0;
}

void start_cpus() {
	LOCK(&thread_st);
	if (numcpu > 1 && LOW_PAGE == 0) {
		ERROR_PRINTF("Couldn't find a low page to host the application processor trampoline, no SMP for you\r\n");
		return;
	}

	uintptr_t scratch;
	if (!kern_mmap(&scratch, (void *)LOW_PAGE, PAGE_SIZE, PROT_WRITE | PROT_READ | PROT_KERNEL | PROT_FORCE, 0)) {
		halt("couldn't map LOW_PAGE", 0);
	}
	memcpy((void *)LOW_PAGE, &ap_trampoline, (uintptr_t)&ap_trampoline2 - (uintptr_t)&ap_trampoline);
	uint8_t trampoline_page = LOW_PAGE / PAGE_SIZE;

	int i = 0;
	for (; i < numcpu; ++i) {
		if ((cpus[i].flags & (CPU_ENABLED|CPU_STARTED)) == CPU_ENABLED) {
			if (start_cpu(i, trampoline_page)) {
				break;
			}
		}
	}
	if (i == numcpu) {
		cpus_initing = 0;
	}
	UNLOCK(&thread_st);
}

_Noreturn
void start_ap() {
	enable_sse();
	kern_mmap_enable_paging();
	unsigned int my_apic_id = local_apic_read(0x20) >> 24;
	size_t cpu_to_start = -1;
	size_t this_cpu = -1;
	for (int i = 0; i < numcpu; ++i) {
		if (cpus[i].apic_id == my_apic_id) {
			this_cpu = i;
			cpus[i].flags |= CPU_STARTED;
			atomic_fetch_add_explicit(&cpusonline, 1, memory_order_acq_rel);
		} else if (cpu_to_start == -1 && (cpus[i].flags & (CPU_ENABLED|CPU_STARTED)) == CPU_ENABLED) {
			cpu_to_start = i;
		}
	}
	// need to setup GS segment before we can print, because of locking
	uint16_t gs_segment = (GDT_GSBASE_OFFSET + 2 * this_cpu) * sizeof(GDT[0]);
	asm volatile ("mov %0, %%gs" :: "a"(gs_segment));
	current_cpu = this_cpu;
	current_thread = cpus[current_cpu].current_thread;

	//ERROR_PRINTF("my apic id %x\r\n", my_apic_id);

	uint64_t base_msr = readmsr(0x1b);

	if (!(base_msr & 0x800)) {
		halt("an enabled local APIC is required\r\n", 0);
	}
	if (local_apic != (base_msr & (~((1 << 12) - 1)))) {
		ERROR_PRINTF("local_apic from APIC %08x does not match value from MSR %08x\r\n", local_apic, base_msr & (~((1 << 12) - 1)));
		halt(NULL, 0);
	}

	// vector spurious intererrupts to 0xFF and enable APIC
	local_apic_write(0xF0, 0x1FF);
	asm volatile ( "lidt %0" :: "m" (IDTR) );
	uint16_t active_tss = ((GDT_TSS_OFFSET + this_cpu) * sizeof(GDT[0])) | 0x3;
	asm volatile ("ltr %0" :: "a"(active_tss));

	threads[current_thread].start = __rdtsc();
	ap_clock_setup();
	arm_timer(0);
	LOCK(&thread_st);
	switch_ap_thread(threads[current_thread].kern_stack_cur);
}

void check_cpuid()
{
	int cont = 1;
	unsigned int a, b, c, d;
	__get_cpuid(1, &a, &b, &c, &d);
	if (! (d & bit_SSE)) {
		ERROR_PRINTF("CPU support for SSE is required\r\n");
		cont = 0;
	}
	if (__get_cpuid(0x80000007, &a, &b, &c, &d) == 0) {
		ERROR_PRINTF("CPU should support cpuid leaf 0x80000007\r\n");
	} else if (!(d & 0x100)) {
		ERROR_PRINTF("CPU TSC should be invariant %X\r\n", d);
	}

	if (!cont) {
		halt("cpu requirements unmet, halting\r\n", 1);
	}
}

// This is our kernel's main function
void kernel_main(uint32_t mb_magic, multiboot_info_t *mb)
{
	// uptime 0
	global_tsc = __rdtsc();
	// We're here! Let's initiate the terminal and display a message to show we got here.
	// Initiate terminal
	term_init();
	setup_fds();
 
	// Display some messages
	ERROR_PRINTF("Hello, World!\r\n");
	check_cpuid();
	enable_sse();
	interrupt_setup();
	clock_setup();

	get_time();

	uintptr_t scratch;
	
	kern_mmap_init(mb->mmap_length, mb->mmap_addr);
	if (!kern_mmap(&scratch, (void *)local_apic, PAGE_SIZE, PROT_KERNEL | PROT_READ | PROT_WRITE | PROT_FORCE, 0)) {
		halt("couldn't map space for Local APIC\r\n", 1);
	}
	for (size_t i = 0; i < io_apic_count; ++i) {
		if (!kern_mmap(&scratch, (void *)io_apics[i].address, PAGE_SIZE, PROT_KERNEL | PROT_READ | PROT_WRITE | PROT_FORCE, 0)) {
			halt("couldn't map space for IO-APIC\r\n", 1);
		}
	}
	setup_cpus();
	rand_init();
	asm volatile ("sti" ::); // enable interrupts here
	init_cpus();
	unsigned int cpus_inited = TIMER_COUNT;
	
	ERROR_PRINTF("kernel read-only %08x - %08x\r\n", &__executable_start, &__etext);

	if (!kern_mmap(&scratch, &__executable_start, &__etext - &__executable_start, PROT_KERNEL | PROT_READ, 0)) {
		halt("couldn't map read only kernel section\r\n", 1);
	}

	ERROR_PRINTF("kernel read-write %08x - %08x\r\n", &__data_start, &__edata);
	if (!kern_mmap(&scratch, &__data_start, &__edata - &__data_start, PROT_KERNEL | PROT_READ | PROT_WRITE, 0)) {
		halt("couldn't map read/write kernel section\r\n", 1);
	}
	
	if (!kern_mmap(&scratch, (void *)vga_buffer, VGA_BUFFER_SIZE, PROT_KERNEL | PROT_FORCE | PROT_READ | PROT_WRITE, 0)) {
		ERROR_PRINTF("couldn't map vga buffer\r\n");
	}
	
	DEBUG_PRINTF("kernel main at %08x\r\n", kernel_main);
	DEBUG_PRINTF("Multiboot magic: %08x (%s)\r\n", mb_magic, (char *) mb->boot_loader_name);
	DEBUG_PRINTF("Multiboot info at %08x (%08x)\r\n", mb, &mb);
	DEBUG_PRINTF("mem range: %08x-%08x\r\n", mb->mem_lower, mb->mem_upper);
	DEBUG_PRINTF("modules: %d @ %08x\r\n", mb->mods_count, mb->mods_addr);
	
	DEBUG_PRINTF("command line: %s\r\n", mb->cmdline);
	char * filestart = strchrnul((char *)mb->cmdline, ' ');
	while (*filestart == ' ') { ++filestart; }
	char * fileend = strchrnul(filestart, ' ');
	char filename [256];
	strncpy(filename, filestart, fileend - filestart);
	DEBUG_PRINTF("file to load %s\r\n", filename);

	size_t mods_count = mb->mods_count;
	multiboot_module_t *mods = (void *)mb->mods_addr;

	kern_mmap_enable_paging();

	kern_mmap(&scratch, (void *) mods, mods_count * sizeof(mods), PROT_KERNEL | PROT_READ | PROT_FORCE, 0);
	for (int mod = 0; mod < mods_count; ++mod) {
		DEBUG_PRINTF("Module %d (%s):\r\n 0x%08x-0x%08x\r\n", mod, mods[mod].cmdline, mods[mod].mod_start, mods[mod].mod_end);
		init_files(&mods[mod]);
	}
	
	struct hardcoded_file * file = find_file(filename);
	if (file) {
		DEBUG_PRINTF("loading %s at %08x\r\n", filename, file->start);
		load_file(file->start, file->name, file->size);
	}
	TAILQ_INIT(&runqueue);
	TAILQ_INIT(&timequeue);
	TAILQ_INIT(&pollqueue);

	while (TIMER_COUNT == cpus_inited) {
		asm volatile ("hlt" ::); // wait for at least one timer interrupt
	}
	start_cpus();

	if (entrypoint) {
		setup_entrypoint();
	}
	halt("end of kernel!", 0);
}

#define THREAD_STATE_ENUM_STRING(NAME) case NAME: return #NAME;

const char *thread_state_name(thread_state state) {
  switch(state) {
    THREAD_STATE_ENUM(THREAD_STATE_ENUM_STRING)
    default: return "unknown";
  }
}

void summary()
{
	uint64_t now = __rdtsc();
	ERROR_PRINTF("%12s %16s %16s %16s %16s\r\n",
		"lock summary", "count", "cycles", "contention", "cycles");
	for (struct lock *l = (struct lock*) &__locks_start; l != &__locks_end; ++l) {
		ERROR_PRINTF("%12s %16llu %16llu %16llu %16llu\r\n", l->name, l->lock_count, l->lock_time, l->contend_count, l->contend_time);
	}

	ERROR_PRINTF("\r\n%20s %15s %16s\r\n", "thread summary", "state", "cycles");
	for (size_t i = 0; i < MAX_THREADS; ++i) {
		if (threads[i].state != EMPTY) {
			if (threads[i].start != 0) {
				threads[i].time += now - threads[i].start;
				threads[i].start = 0;
			}
			ERROR_PRINTF("%20s %15s %16llu\r\n", threads[i].name, thread_state_name(threads[i].state), threads[i].time);
		}
	}
	uint64_t fpt = fixed_point_time(0);
	uint64_t seconds = FIXED_POINT_SECONDS(fpt);
	ERROR_PRINTF("\r\ncrazierl ran for %llu seconds\r\n", seconds);
	while (com_buffer_idle() == 0) {
	}
}

void RELOCK(struct lock * lock, size_t target)
{
	__sync_synchronize();
	if (current_thread + 1 != lock->locked) {
		ERROR_PRINTF("lock %s(%p) relocked by wrong thread %d != %d -> %d\r\n", lock->name, &(lock->locked), current_thread, lock->locked - 1, target);
		halt("halting\r\n", 0);
	}
	lock->locked = target + 1;
}
void LOCK(struct lock * lock)
{
	int first = 1;
	uint64_t start = __rdtsc();
	size_t lock_token = current_thread + 1;

	if (lock->locked == lock_token) {
		term_print("!recursive lock!\rn\n");
		while (1) { }
	}
	while (!__sync_bool_compare_and_swap(& lock->locked, 0, lock_token)) {
		first = 0;
		_mm_pause();
	}
	__sync_synchronize();
	if (lock->start != 0) {
		halt("lock start should be zero when locked\r\n", 0);
	}
	if (first) {
		lock->start = start;
	} else {
		lock->start = __rdtsc();
		lock->contend_time += lock->start - start;
		++(lock->contend_count);
	}
	++(lock->lock_count);
}

void UNLOCK(struct lock * lock)
{
	uint64_t end = __rdtsc();
	size_t lock_token = current_thread + 1;
	lock->lock_time += end - lock->start;
	lock->start = 0;
	__sync_synchronize();
	if (lock_token != lock->locked) {
		ERROR_PRINTF("lock %s(%p) unlocked by wrong thread %d != %d\r\n", lock->name, &(lock->locked), current_thread, lock->locked - 1);
		halt("halting\r\n", 0);
	}
	lock->locked = 0;
}

void ASSERT_LOCK(struct lock * lock)
{
	size_t lock_token = current_thread + 1;
	__sync_synchronize();
	if (lock_token != lock->locked) {
		ERROR_PRINTF("lock %s(%p) locked by wrong thread %d != %d\r\n", lock->name, &(lock->locked), current_thread, lock->locked - 1);
		halt("halting\r\n", 0);
	}
}
