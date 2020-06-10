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

#define CPU_ENABLED (1 << 0)
#define CPU_STARTED (1 << 1)
// cpu is running idle task OR did not switch tasks at last timer
// used to decide which cpu to wake when a thread becomes runnable
#define CPU_IDLE    (1 << 2)

struct cpu {
    uint32_t apic_id;
    uint32_t volatile flags;
};

#define MAX_CPUS 256
extern struct cpu cpus[];
