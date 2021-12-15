//#ifndef _APIC_H
//#define _APIC_H
#include <stdint.h>
#include <stddef.h>


#define MAX_IO_APICS 8
#define CLOCK_MS    10
extern uint8_t timer_gsirq;
extern uint8_t timer_flags;
extern uintptr_t local_apic;
extern size_t io_apic_count;

struct io_apic {
    uint32_t volatile *address;
    unsigned int base;
    unsigned int numintr;
};


extern struct io_apic io_apics[];

void local_apic_write(unsigned int reg, uint32_t value);
uint32_t local_apic_read(unsigned int reg);
void ioapic_set_gsi_vector(unsigned int irq, uint8_t flags, uint8_t vector, uint8_t physcpu);
void pic_setup(int master_offset, int slave_offset);
void clock_setup();

#define FIXED_POINT_TIME_NANOSECOND(seconds, nanoseconds) (((uint64_t) seconds << 24) + (((uint64_t) nanoseconds << 24) / 1000000000))
#define FIXED_POINT_SECONDS(fpt)(fpt >> 24)
#define FIXED_POINT_MILLISECONDS(fpt) (((fpt & ((1 << 24) -1)) * 1000) >> 24)
#define FIXED_POINT_MICROSECONDS(fpt) (((fpt & ((1 << 24) -1)) * 1000000) >> 24)
#define FIXED_POINT_NANOSECONDS(fpt)  (((fpt & ((1 << 24) -1)) * 1000000000) >> 24)
#define TSC_TICK_SCALE 32

extern uint64_t SCALED_S_PER_TSC_TICK;

//#endif