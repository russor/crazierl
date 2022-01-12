// enum magic from https://kubyshkin.name/posts/c-language-enums-tips-and-tricks/
#define THREAD_STATE_ENUM(VARIANT) \
    VARIANT(EMPTY) \
    VARIANT(RUNNABLE) \
    VARIANT(INITING) \
    VARIANT(RUNNING) \
    VARIANT(UMTX_MUTEX_WAIT) \
    VARIANT(UMTX_WAIT) \
    VARIANT(IO_READ) \
    VARIANT(IO_WRITE) \
    VARIANT(WAIT_FOREVER) \
    VARIANT(POLL) \
    VARIANT(IDLE)

#define THREAD_STATE_ENUM_VARIANT(NAME) NAME,

typedef enum {
    THREAD_STATE_ENUM(THREAD_STATE_ENUM_VARIANT)
} thread_state;
    
struct crazierl_thread {
    uint64_t timeout;
    uint64_t time;
    uint64_t start;
    cpuset_t cpus;
    thread_state state;
    uintptr_t kern_stack_top;
    uintptr_t kern_stack_cur;
    uintptr_t tls_base;
    uintptr_t wait_target;
    char name [MAXCOMLEN + 1];
};

