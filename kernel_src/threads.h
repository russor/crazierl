#include <x86/fpu.h>

#define THREAD_EMPTY 0
#define THREAD_RUNNABLE 1
#define THREAD_INITING 2
#define THREAD_RUNNING 3


struct crazierl_thread {
    unsigned int state;
    uintptr_t kern_stack_top;
    uintptr_t kern_stack_cur;
    uintptr_t tls_base;
    union savefpu savearea;
};

