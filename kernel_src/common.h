#ifndef _COMMON_H
#define _COMMON_H

#include <sys/param.h>
#include <sys/types.h>


//#define DEBUG_PRINTF(...) term_printf(__VA_ARGS__); move_cursor()
#define DEBUG_PRINTF(...)
#define ERROR_PRINTF(...) term_printf(__VA_ARGS__); move_cursor()
void move_cursor();
void term_printf(const char *, ...);

#endif
