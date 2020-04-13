#include <x86/fpu.h>

#define THREAD_EMPTY 0
#define THREAD_RUNNABLE 1
#define THREAD_INITING 2
#define THREAD_RUNNING 3
#define THREAD_UMTX_MUTEX_WAIT 4
#define THREAD_UMTX_WAIT 5
#define THREAD_IO_READ 6
#define THREAD_IO_WRITE 6

struct crazierl_thread {
    unsigned int state;
    uintptr_t kern_stack_top;
    uintptr_t kern_stack_cur;
    uintptr_t tls_base;
    union savefpu savearea;
    uintptr_t wait_target;
};
