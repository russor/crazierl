#include "threads.h"

extern unsigned int numcpu;

void * acpi_find_rsdt (void *);
int acpi_check_table(void *);
int acpi_process_madt(void *);
void * acpi_find_table(void *, void *);

#define CPU_ENABLED (1 << 0)
#define CPU_STARTED (1 << 1)
// cpu is running idle task OR did not switch tasks at last timer
// used to decide which cpu to wake when a thread becomes runnable
#define CPU_IDLE    (1 << 2)

struct cpu {
    uint32_t apic_id;
    uint32_t volatile flags;
    uint32_t volatile current_thread;
    struct threadqueue runqueue;
    struct threadqueue timequeue;
    uint32_t clock_tick;
    uint64_t timeout;
    uint64_t last_time;
};

#define MAX_CPUS 256
extern struct cpu cpus[];
