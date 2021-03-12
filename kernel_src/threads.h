
#define THREAD_EMPTY 0
#define THREAD_RUNNABLE 1
#define THREAD_INITING 2
#define THREAD_RUNNING 3
#define THREAD_UMTX_MUTEX_WAIT 4
#define THREAD_UMTX_WAIT 5
#define THREAD_IO_READ 6
#define THREAD_IO_WRITE 7
#define THREAD_WAIT_FOREVER 8
#define THREAD_POLL 9
#define THREAD_IDLE 10

struct crazierl_thread {
    uint64_t timeout;
    cpuset_t cpus;
    unsigned int state;
    uintptr_t kern_stack_top;
    uintptr_t kern_stack_cur;
    uintptr_t tls_base;
    uintptr_t wait_target;
    char name [MAXCOMLEN + 1];
};

