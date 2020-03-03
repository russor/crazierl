#ifndef _COMMON_H
#define _COMMON_H

#include <sys/param.h>
#include <sys/types.h>
#include <printf.h>

#define DEBUG_PRINTF(...) printf(__VA_ARGS__); move_cursor()
//#define DEBUG_PRINTF(...)
#define ERROR_PRINTF(...) printf(__VA_ARGS__); move_cursor()
void move_cursor();

#endif
