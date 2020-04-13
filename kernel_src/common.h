#ifndef _COMMON_H
#define _COMMON_H

#include <sys/param.h>
#include <sys/types.h>


#ifdef CRAZIERL_KERNEL
#define DEBUG_PRINTF(...) term_printf(__VA_ARGS__); move_cursor()
//#define DEBUG_PRINTF(...)
#define ERROR_PRINTF(...) term_printf(__VA_ARGS__); move_cursor()
#endif

#ifdef CRAZIERL_USER
//#define DEBUG_PRINTF(...) printf(__VA_ARGS__);
#define DEBUG_PRINTF(...)
#define ERROR_PRINTF(...) printf(__VA_ARGS__);
#endif

void move_cursor();
void term_printf(const char *, ...);

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
