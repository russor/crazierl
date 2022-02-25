#ifndef _COMMON_H
#define _COMMON_H

#include <sys/param.h>
#include <sys/types.h>


#ifdef CRAZIERL_KERNEL
//#define DEBUG_PRINTF(...) term_printf(__VA_ARGS__); move_cursor()
#define DEBUG_PRINTF(...)
#define ERROR_PRINTF(...) term_printf(__VA_ARGS__); move_cursor()
#define EARLY_ERROR_PRINTF(...) early_term_printf(__VA_ARGS__)

#define FIRST_IRQ_VECTOR 0x20
#define TIMER_VECTOR FIRST_IRQ_VECTOR
#define SWITCH_VECTOR (FIRST_IRQ_VECTOR + 1)
#define HALT_VECTOR (FIRST_IRQ_VECTOR + 2)

struct lock {
	volatile uint32_t locked;
	char	 name[12];
	uint64_t lock_time;
	uint64_t lock_count;
	uint64_t contend_time;
	uint64_t contend_count;
	uint64_t start;
};

#define DECLARE_LOCK(X) struct lock X __attribute((__section__("locks"))) = { .name = #X}

void RELOCK(struct lock *, size_t target);
void LOCK(struct lock *);
void UNLOCK(struct lock *);
void ASSERT_LOCK(struct lock *);

void rand_init();
void rand_bytes(void * out, size_t len);
void rand_update(const void * seed, size_t len);

extern __thread size_t current_thread;
extern __thread size_t current_cpu;
_Noreturn void halt(char * message, int dontpropagate);
void move_cursor();
void term_printf(const char *, ...);
void early_term_printf(const char *, ...);

#endif

#ifndef likely
#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#endif /* likely and unlikely */

static inline unsigned int min(unsigned int a, unsigned int b) {
	if (a < b) { return a; }
	return b;
}

static inline unsigned int max(unsigned int a, unsigned int b) {
	if (a > b) { return a; }
	return b;
}

#endif
