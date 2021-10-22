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

//#endif