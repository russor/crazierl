#include <sys/queue.h>
#include <sys/cpuset.h>

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

TAILQ_HEAD(threadqueue, crazierl_thread);


#define THREAD_PINNED (1 << 0)

struct crazierl_thread {
    TAILQ_ENTRY(crazierl_thread) runq;
    TAILQ_ENTRY(crazierl_thread) timeq;
    TAILQ_ENTRY(crazierl_thread) waitq;
    struct threadqueue *waitqhead;
    uint64_t timeout;
    uint64_t time;
    uint64_t start;
    cpuset_t cpus;
    thread_state state;
    uintptr_t kern_stack_top;
    uintptr_t kern_stack_cur;
    uintptr_t tls_base;
    uintptr_t wait_target;
    uint32_t flags; // copied in thr_new
    char name [MAXCOMLEN + 1];
};

