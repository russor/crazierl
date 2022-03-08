#ifndef _KERNEL_MMAP_H
#define _KERNEL_MMAP_H

#include <sys/mman.h>

#define PROT_KERNEL 0x1000
#define PROT_FORCE  0x2000
#define MAP_EARLY MAP_RESERVED0020 /* reuse reserved flag to skip locking */

#define PAGE_FLOOR(a) ((uintptr_t)(a) & ~(PAGE_SIZE - 1))
#define PAGE_CEIL(a) (((uintptr_t)(a) & (PAGE_SIZE - 1)) ? PAGE_FLOOR((uintptr_t)(a) + PAGE_SIZE - 1): (uintptr_t)(a))

void kern_mmap_init(unsigned int, unsigned int, uintptr_t);
void kern_munmap(uint16_t, uintptr_t, uintptr_t);
int kern_mmap(uintptr_t *, void *, size_t, int, int);
void kern_mmap_enable_paging();
void kern_mmap_debug (uintptr_t);
uintptr_t kern_mmap_physical(uintptr_t);

extern uintptr_t LOW_PAGE;

#endif
