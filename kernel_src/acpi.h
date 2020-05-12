extern uintptr_t local_apic;
extern unsigned int numcpu;
extern uint8_t timer_gsirq;
extern uint8_t timer_flags;

void * acpi_find_rsdt (void *);
int acpi_check_table(void *);
int acpi_process_madt(void *);
void * acpi_find_table(void *, void *);

struct io_apic {
    uint32_t volatile *address;
    unsigned int base;
    unsigned int numintr;
};

extern size_t io_apic_count;
extern struct io_apic io_apics[];
