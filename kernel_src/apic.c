#include "apic.h"
#include "common.h"
#include "acpi.h"

// Ideally things related to APIC (Advanced Programmable Interrupt Controller), but also
// includes other things in the arena of general PIC and interrupts, so more thematic
// than anything

#define APIC_DISABLE	0x10000
#define APIC_TIMER_PERIODIC	0x20000

#define APIC_TPR		0x080	// Task Priority Register
#define APIC_LDR		0x0D0
#define APIC_DFR		0x0E0
#define APIC_SPURIOUS	0x0F0
#define APIC_LVT_TIMER	0x320
#define APIC_LVT_PERF	0x340
#define APIC_TIMER_INITIAL	0x380
#define APIC_TIMER_CURRENT	0x390
#define APIC_TIMER_DIVIDE	0x3E0

// https://wiki.osdev.org/8259_PIC
#define PORT_PIC1_CMD 	0x20
#define PORT_PIC1_DATA 	0x21
#define PORT_PIC2_CMD	0xA0
#define PORT_PIC2_DATA	0xA1

#define PIC_INTERRUPT_ACK 0x20

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
uintptr_t local_apic;
uint8_t timer_gsirq;
uint8_t timer_flags;
unsigned int io_apic_count;
uint64_t SCALED_S_PER_TSC_TICK;
uint32_t clock_ticks;

//extern struct io_apic io_apics[];

struct io_apic io_apics[MAX_IO_APICS];

void local_apic_write(unsigned int reg, uint32_t value)
{
	*(uint32_t *)(local_apic + reg) = value;
}

uint32_t local_apic_read(unsigned int reg) {
	return *(uint32_t *)(local_apic + reg);
}

void ioapic_set_gsi_vector(unsigned int irq, uint8_t flags, uint8_t vector, uint8_t physcpu) {
	int ioapic = 0;
	unsigned int origirq = irq;
	while (ioapic < io_apic_count) {
		if (irq < io_apics[ioapic].numintr) {
			uint8_t index = (irq * 2) + 0x10;
			uint32_t lo, hi;

			lo = vector;
			if (flags & 8) { // level triggered
				lo |= 0x8000;
			}
			if (flags & 2) { // active low
				lo |= 0x2000;
			}
			hi = physcpu << 24;
			
			io_apics[ioapic].address[0] = index;
			io_apics[ioapic].address[4] = lo;
			io_apics[ioapic].address[0] = index + 1;
			io_apics[ioapic].address[4] = hi;
			return;
		} else {
			irq -= io_apics[ioapic].numintr;
		}
		++ioapic;
	}
	ERROR_PRINTF("couldn't find IO-APIC for irq %d\r\n", origirq);
	halt(NULL, 0);
}

#define PIT_CH2_GATE	0x61
#define PIT_CH0_DATA	0x40
#define PIT_CH1_DATA	0x41
#define PIT_CH2_DATA	0x42
#define PIT_CMD			0x43

#define PIT_OUT			0x20
#define PIT_GATE		0x01

void load_pit(uint16_t ms)
{
	// 3579645 /3 Hz= 1193182 Hz = 1.193182 Mhz
	// time_in_ms = val / (3579545/3) * 1000
	// time_in_ms / 1000 = val / (3579545/3)
	// (time_in_ms / 1000) * (3579545/3) = val
	// (time_in_ms / 3000) * 3579545 = val

	// iow 10ms = 3579545 / 300

	// Tell PIT to sleep for 10ms
	// 65536 = 54.878 ms
	// https://wiki.osdev.org/Programmable_Interval_Timer


	uint16_t counter = (ms * 3579545) / 3000;

	uint8_t existing = inb(PIT_CH2_GATE);
	existing = 0xFF & ((existing & 0xfd) | 1); // disable speaker + counting 
	// channel 2 data port = 0x61
	outb(PIT_CH2_GATE, existing);
	// 0 = binary mode
	// 001 = hardware re-triggerable one-shot
	// 11 = lobyte/hibyte
	// 10 = channel 2
	//0xB2

	// 0x61 bits
	// 7 (ro) = memory parity
	// 6 (ro) = i/o check
	// 5 (ro) = out2
	// 4 (ro) = out1
	// 3 (rw) = i/o check enable
	// 2 (rw) = memory parity check enable
	// 1 (rw) = speaker 
	// 0 (rw) = gate2
	//uint8_t val = (((2 << 2) || 3) << 4) | (2);
	outb(PIT_CMD,  0b10110000); // 0x43 = Command regisger


	existing = inb(PIT_CH2_GATE); // gate low

	outb(PIT_CH2_DATA, counter & 0xFF); 
	outb(PIT_CH2_DATA, 0xFF & ((counter & 0xFF00) >> 8));

	// Start counting
	existing = 0xFF & inb(PIT_CH2_GATE); 
	// create rising edge by toggling value down
	outb(PIT_CH2_GATE, existing & 0xFE);  
	inb(PIT_CH2_GATE+1);
	outb(PIT_CH2_GATE, existing | 1); // gate hight

}

void pit_wait()
{
	uint8_t val = 0;
	do {
		val = inb(PIT_CH2_GATE);
	} while ( ((val & 0x20) == 0)) ;
}


void arm_timer(uint64_t wait)
{
	cpus[current_cpu].timeout = wait;
	if (wait == 0) {
		cpus[current_cpu].clock_tick = clock_ticks;
		local_apic_write(APIC_TIMER_INITIAL, clock_ticks);
	} else {
		uint64_t ticks = wait * clock_ticks;
		ticks /= FIXED_POINT_TIME_NANOSECOND(0, CLOCK_MS * 1000000);
		if (ticks > 0xFFFFFFFF) {
			ticks = 0xFFFFFFFF;
		}
		cpus[current_cpu].clock_tick = ticks & 0xFFFFFFFF;
		local_apic_write(APIC_TIMER_INITIAL, ticks & 0xFFFFFFFF);
	}
}

void ap_clock_setup()
{
	local_apic_write(APIC_TIMER_INITIAL, clock_ticks);
	local_apic_write(APIC_LVT_TIMER, TIMER_VECTOR);
	local_apic_write(APIC_TIMER_DIVIDE, 0x3); // div 16
}

// Initially based on https://wiki.osdev.org/APIC_timer
void clock_setup()
{
	ERROR_PRINTF("Starting clock timer calibration\r\n");
	// divider 16
	local_apic_write(APIC_LVT_TIMER, TIMER_VECTOR);
	local_apic_write(APIC_TIMER_DIVIDE, 0x3);

	uint16_t calibms = 10;
	load_pit(calibms);
	local_apic_write(APIC_TIMER_INITIAL, 0xFFFFFFFF);
	uint64_t tsc_start = __rdtsc();

	pit_wait();

	local_apic_write(APIC_LVT_TIMER, APIC_DISABLE);
	uint64_t tsc_end = __rdtsc();

	uint64_t ticks = 0xFFFFFFFF - local_apic_read(APIC_TIMER_CURRENT);

	ERROR_PRINTF("Clock: %llu apic ticks have occurred in %d ms\r\n", ticks, calibms);

	uint64_t tsc_ticks_per_s = (ticks * 16 * 1000) / calibms;
	ERROR_PRINTF("Clock: Est speed (Mhz): %llu\r\n", tsc_ticks_per_s / 1000000);

	uint64_t tsc_ticks = tsc_end - tsc_start;
	ERROR_PRINTF("Clock: %llu tsc ticks have occurred in %d ms\r\n", tsc_ticks, calibms);
	tsc_ticks_per_s = (tsc_ticks * 1000) / calibms;
	ERROR_PRINTF("Clock: Est speed (Mhz): %llu\r\n", tsc_ticks_per_s / 1000000);
	SCALED_S_PER_TSC_TICK = ((FIXED_POINT_TIME_NANOSECOND(1, 0) << TSC_TICK_SCALE)* calibms / 1000) / tsc_ticks ;
	ERROR_PRINTF("(scaled) fixedpoint time per tsc_tick %llu\r\n", SCALED_S_PER_TSC_TICK);

	ERROR_PRINTF("fixed point time in %llu ticks: %llu ns\r\n", tsc_ticks, FIXED_POINT_NANOSECONDS((SCALED_S_PER_TSC_TICK * tsc_ticks) >> TSC_TICK_SCALE));

	// Ok, now we disable the PIT timer by putting it into one-shot mode and not firing it
	outb(PIT_CMD,  0b00110000); // 0x43 = Command regisger

	clock_ticks = (ticks * CLOCK_MS) / calibms;
	ERROR_PRINTF("Clock: Will interrupt every %d ms using %u\r\n", CLOCK_MS, clock_ticks);

	local_apic_write(APIC_TIMER_INITIAL, clock_ticks);
	local_apic_write(APIC_LVT_TIMER, TIMER_VECTOR);
	local_apic_write(APIC_TIMER_DIVIDE, 0x3); // div 16
}

void pic_setup(int master_offset, int slave_offset)
{

	// setup PIC before disabling, so spurious interrupts hit vector 0xF7
	outb(PORT_PIC1_CMD, 0x11); // request initialization
	outb(PORT_PIC2_CMD, 0x11); // request initialization
	outb(PORT_PIC1_DATA, master_offset);
	outb(PORT_PIC2_DATA, slave_offset); // offset interrupts by 0xF0
	outb(PORT_PIC1_DATA, 0x4); // indicate slave PIC on IRQ 2
	outb(PORT_PIC2_DATA, 0x2); // indicate slave PIC is slave
	outb(PORT_PIC1_DATA, 0x01); // set to 8086 mode
	outb(PORT_PIC2_DATA, 0x01); // set to 8086 mode
	outb(PORT_PIC1_DATA, 0xFF); // mask all interrupts
	outb(PORT_PIC2_DATA, 0xFF); // mask all interrupts
}
