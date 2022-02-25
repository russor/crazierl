#ifndef _KERNEL_MMAP_H
#define _KERNEL_MMAP_H

#include <sys/mman.h>

#define PROT_KERNEL 0x1000
#define PROT_FORCE  0x2000
#define MAP_EARLY MAP_RESERVED0020 /* reuse reserved flag to skip locking */

void kern_mmap_init(unsigned int, unsigned int);
void kern_munmap(uint16_t, uintptr_t, uintptr_t);
int kern_mmap(uintptr_t *, void *, size_t, int, int);
void kern_mmap_enable_paging();
void kern_mmap_debug (uintptr_t);
uintptr_t kern_mmap_physical(uintptr_t);

extern uintptr_t LOW_PAGE;

#endif
