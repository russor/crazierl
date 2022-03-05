#include "common.h"
#include "kern_mmap.h"

#include "/usr/src/stand/i386/libi386/multiboot.h"
#include <stdint.h>
#include <strings.h>
#include <errno.h>
#include <sys/queue.h>

#define ONE_MB 0x00100000
#define PAGE_DIRECTORY_BITS 22
#define PAGE_LEVEL_BITS 12
#define PAGE_LEVEL_MASK 0x3FF

#define PAGE_PRESENT 0x01
#define PAGE_READWRITE 0x02
#define PAGE_USER 0x04
#define PAGE_SIZE_FLAG 0x80
// 0x200, 0x400, 0x800 flags are available for use by software
#define PAGE_BOTH_UK 0x200
#define PAGE_FORCE 0x400

#define MAX_MEM_SEGMENTS 128
#define MAX_PAGE_ORDER 12
#define FREELIST_COUNT MAX_PAGE_ORDER

uintptr_t PAGE_DIRECTORY;
uintptr_t PAGE_TABLE_BASE;
uintptr_t LEAST_ADDR;
uintptr_t MAX_ADDR;
uintptr_t LOW_PAGE;
int PAGE_SETUP_FINISHED;

struct mem_segment {
	uintptr_t addr;
	size_t len;
};

struct page {
	uint8_t order;
	uint8_t free;
	LIST_ENTRY(page) freelist;
};

struct mem_segment mem_segments[MAX_MEM_SEGMENTS];
int mem_segment_count;

LIST_HEAD(freelist, page) freelists[FREELIST_COUNT];

uint32_t * pagetable_direntry (uintptr_t logical)
{
	return (uint32_t *) (PAGE_DIRECTORY + ((logical >> PAGE_DIRECTORY_BITS) * sizeof(uint32_t)));
}

uint32_t * pagetable_entry (uint32_t direntry, uintptr_t logical)
{
	uintptr_t lookup = ((direntry & ~(PAGE_SIZE -1)) | (((logical >> PAGE_LEVEL_BITS) & PAGE_LEVEL_MASK) * sizeof(uint32_t)));
	return (uint32_t *) lookup;
}

uintptr_t kern_mmap_physical(uintptr_t addr) {
	return addr; // FIXME: broken if mappings aren't 1:1
}

void kern_mmap_debug(uintptr_t addr) {
	uint32_t *direntry = pagetable_direntry(addr);
	uint32_t *table_entry = pagetable_entry(*direntry, addr);
	ERROR_PRINTF("logical %08x; dir %08x -> %08x; page %08x -> %08x\r\n", addr, direntry, *direntry, table_entry, *table_entry);
}


int mem_available (uintptr_t addr, size_t len)
{
	while (len) {
		uint32_t * page_table_entry = pagetable_entry(*(pagetable_direntry(addr)), addr);
		if (*page_table_entry == 0 || (*page_table_entry & PAGE_PRESENT)) {
			return 0;
		}
		addr += PAGE_SIZE;
		len -= PAGE_SIZE;
	}
	return 1;
}

void add_page_mapping (uint16_t flags, uintptr_t logical, uintptr_t physical) {
	uint32_t * directory_entry = pagetable_direntry(logical);
	if (*directory_entry == 0) {
		if (unlikely(!PAGE_SETUP_FINISHED || (flags & PAGE_FORCE))) {
			*directory_entry = (PAGE_TABLE_BASE + ((logical >> PAGE_DIRECTORY_BITS) * PAGE_SIZE)) | (flags & (PAGE_SIZE -1));
		} else {
			halt("Trying to setup a mapping without a directory entry\r\n", 0);
		}
	}
	
	// update flags
	if ((flags & PAGE_USER) && !(*directory_entry & PAGE_USER)) {
		//DEBUG_PRINTF("Adding user flag to page_directory for %08x -> %08x\r\n", logical, physical);
		*directory_entry |= (PAGE_USER | PAGE_BOTH_UK);
	} else if (!(flags & PAGE_USER) && (*directory_entry & PAGE_USER)) {
		//DEBUG_PRINTF("Adding both flag to page_directory for %08x -> %08x\r\n", logical, physical);
		*directory_entry |= (PAGE_USER | PAGE_BOTH_UK);
	}
	
	if ((flags & PAGE_READWRITE) && !(*directory_entry & PAGE_READWRITE)) {
		*directory_entry |= PAGE_READWRITE;
	}
	if ((flags & PAGE_PRESENT) && !(*directory_entry & PAGE_PRESENT)) {
		*directory_entry |= PAGE_PRESENT;
	}
	
	uint32_t *table_entry = pagetable_entry(*directory_entry, logical);
	
	if (*table_entry == 0) {
		if (unlikely(!PAGE_SETUP_FINISHED || (flags & PAGE_FORCE))){
			*table_entry = (physical & ~(PAGE_SIZE -1)) | (flags & (PAGE_SIZE - 1));
		} else {
			DEBUG_PRINTF("table_entry %p (%x), directory_entry %p (%x), logical %p\r\n",
				table_entry, *table_entry, directory_entry, *directory_entry, logical);
			halt("Trying to setup a mapping without a page table entry\r\n", 0);
		}
	}
	// update flags
	if ((flags & PAGE_USER) && !(*table_entry & PAGE_USER)) {
		if (*table_entry & (PAGE_PRESENT)) {
			//DEBUG_PRINTF("Adding user flag to page_directory for %08x -> %08x\r\n", logical, physical);
			*table_entry |= (PAGE_USER | PAGE_BOTH_UK);
		} else {
			*table_entry |= PAGE_USER;
		}
	} else if (!(flags & PAGE_USER) && (*table_entry & PAGE_USER)) {
		//DEBUG_PRINTF("Adding both flag to page_directory for %08x -> %08x\r\n", logical, physical);
		*table_entry |= PAGE_BOTH_UK;
	}

	if ((flags & PAGE_READWRITE) && !(*table_entry & PAGE_READWRITE)) {
		*table_entry |= PAGE_READWRITE;
	}
	if ((flags & PAGE_PRESENT) && !(*table_entry & PAGE_PRESENT)) {
		*table_entry |= PAGE_PRESENT;
	}
}

void add_page_mappings (uint16_t mappingflags, uintptr_t addr, size_t len) {
	while (len) {
		add_page_mapping(mappingflags, addr, addr);
		addr += PAGE_SIZE;
		len -= PAGE_SIZE;
	}
}

struct page *get_segment_pages(struct mem_segment *segment) {
	return (struct page *) segment->addr;
}

struct page *get_segment_page(struct mem_segment *segment, uintptr_t addr) {
	if (segment == NULL) {
		return NULL;
	}
	if (addr >= segment->addr && addr + PAGE_SIZE < segment->addr + segment->len) {
		struct page *segment_pages = get_segment_pages(segment);
		return segment_pages[(addr - segment->addr) / PAGE_SIZE];
	}
	return NULL;
}

uintptr_t get_page_addr(struct mem_segment *segment, struct page *page) {
	struct page *segment_pages = get_segment_pages(segment);
	return (page - segment_pages) * PAGE_SIZE;
}

struct mem_segment *get_page_segment(struct page *page) {
	struct mem_segment *segment;
	for (segment = &mem_segments[0]; segment < &mem_segments[mem_segment_count]; segment++) {
		uintptr_t page_struct_addr = (uintptr_t) page;
		uintptr_t segment_first_page_struct_addr = (uintptr_t) get_segment_pages(segment);
		uintptr_t segment_last_page_struct_addr = segment_first_page_struct_addr + segment->len / PAGE_SIZE;
		if (page_struct_addr >= segment_first_page_struct_addr && page_struct_addr < segment_last_page_struct_addr) {
			return segment;
		}
	}
	return NULL;
}

struct mem_segment *get_addr_mem_segment(uintptr_t addr) {
	struct mem_segment *segment;
	for (segment = &mem_segments[0]; segment < &mem_segments[mem_segment_count]; segment++) {
		if (get_segment_page(segment, addr) != NULL) {
                        return segment;
		}
	}
	return NULL;
}

struct page *get_area_buddy(struct page *page) {
	struct mem_segment *segment = get_page_segment(page);
	uintptr_t page_addr = get_page_addr(segment, page);
	uintptr_t buddy_addr = page_addr ^ (PAGE_SIZE << (page->order & PAGE_ORDER_MASK));
        // page buddies can only live in the same memory segment
	return get_segment_page(segment, buddy_addr);
}

struct page *buddy_alloc(uint8_t order) {
	const uint8_t target_order = order;

	if (target_order > MAX_PAGE_ORDER) {
		EARLY_ERROR_PRINTF("attempted huge allocation of order %u", (unsigned int) order);
		halt("attempted huge allocation", 0);
	}

	// find lowest order freelist with an available page
	while (LIST_EMPTY(&freelists[order])) {
		order += 1;
		if (order > MAX_PAGE_ORDER) {
			return NULL;
		}
	}

	// split free areas until we reach the target order
	while (order > target_order) {
		struct page *free_area_first_page = LIST_FIRST(&freelists[order]);
		LIST_REMOVE(free_area_first_page, freelist);

		// demote the order of the first page of the free area and its buddy
		order -= 1;
                free_area_first_page->order = order;
                
		// NB: get_area_buddy needs to be called AFTER demoting the area
		struct page *free_area_buddy_page = get_area_buddy(free_area_first_page);
                free_area_buddy_page->order = order;

		// add the resultant areas of the split to the freelist
		LIST_INSERT_HEAD(&freelists[order], free_area_first_page, freelist);
		LIST_INSERT_HEAD(&freelists[order], free_area_buddy_page, freelist);
	}

	// we are guaranteed at least two free areas on the target freelist now, so remove one
	struct page *free_area = LIST_FIRST(&freelists[target_order]);
        LIST_REMOVE(free_area, freelist);

	free_area->free = 0;
	return free_area;
}

struct page *buddy_free(struct page *area) {
	struct mem_segment segment = get_page_segment(area);
	if (segment == NULL) {
		uintptr_t struct_page_addr = (uintptr_t) area;
		EARLY_ERROR_PRINTF("invalid free of %08x outside memory segment", struct_page_addr);
		halt("invalid free outside memory segment", 0);
	}
	if (area->free) {
		uintptr_t addr = get_page_addr(segment, page);
		EARLY_ERROR_PRINTF("double free of %08x", addr);
		halt("double free", 0);
        }

	// mark the area as free and put on freelist
	area->free = 1;
	LIST_INSERT_HEAD(&freelists[area->order], area, freelist);

	// merge with other areas as much as possible
	while (area->order < MAX_PAGE_ORDER) {
		struct page *buddy = get_area_buddy(area);
		// only merge the buddy is free and of the same order
		if (buddy == NULL || buddy->order != area->order || !buddy->free) {
			break;
		}

		// remove the two merging areas from their freelist
		LIST_REMOVE(area, freelist);
		LIST_REMOVE(buddy, freelist);

		// promote the order of both merging areas
		area->order += 1;
		buddy->order += 1;

		// mark the second area as in-use as it's being merged with the first
		struct page *first_area = MIN(area, buddy);
		struct page *second_area = MAX(area, buddy);
		second_area->free = 0;

		// add the new merged area to the right freelist
		LIST_INSERT_HEAD(&freelists[area->order], first_area, freelist);
	}
}

void add_mem_segment(uintptr_t addr, uintptr_t len) {
	if (mem_segment_count >= MAX_MEM_SEGMENTS) {
		halt("tried to add too many memory segments", 0);
	}

	// initialize the beginning of the segment for struct pages
	uintptr_t page_count = len / PAGE_SIZE;
        size_t page_data_byte_size = sizeof(struct page) * page_count;
	memset(addr, 0, page_data_byte_size);

	struct mem_segment *segment = &mem_segments[mem_segment_count];
	segment->addr = addr;
	segment->len = len;

	mem_segment_count += 1;

	struct page *page;
        size_t page_data_page_count = (page_data_byte_size + (PAGE_SIZE - 1)) / PAGE_SIZE;
	for (page = get_segment_pages(segment) + page_data_page_count; page < page_count; page++) {
		buddy_free(page);
	}
}

DECLARE_LOCK(mmap_lock);

void kern_munmap (uint16_t mode, uintptr_t addr, size_t size)
{
	addr = addr & ~(PAGE_SIZE - 1);
	if (size & (PAGE_SIZE -1)) {
		size = (size & ~(PAGE_SIZE -1)) + PAGE_SIZE;
	}
	DEBUG_PRINTF("unmapping %08x bytes at %08x (mode %x)\r\n", size, addr, mode);
	LOCK(&mmap_lock);
	for (; size; addr += PAGE_SIZE, size -= PAGE_SIZE) {
		int access_changed = 0;
		uint32_t * table_entry = pagetable_entry(*(pagetable_direntry(addr)), addr);
		if ((*table_entry & PAGE_PRESENT) == 0) {
			continue;
		}
		if ((mode & PROT_KERNEL) == 0) {
			switch (*table_entry & (PAGE_USER | PAGE_BOTH_UK)) {
				case PAGE_USER:
					access_changed = 1;
					*table_entry &= ~(PAGE_PRESENT | PAGE_READWRITE | PAGE_USER);
					break;
				case PAGE_USER | PAGE_BOTH_UK:
					access_changed = 1;
					*table_entry &= ~(PAGE_USER | PAGE_BOTH_UK);
			}
		} else {
			switch (*table_entry & (PAGE_USER | PAGE_BOTH_UK)) {
				case PAGE_USER | PAGE_BOTH_UK:
					access_changed = 1;
					*table_entry &= ~PAGE_BOTH_UK;
					break;
				case 0:
					access_changed = 1;
					*table_entry &= ~(PAGE_PRESENT | PAGE_READWRITE);
			}
		}
		if (access_changed) {
			asm volatile("invlpg (%0)" ::"r" (addr) : "memory");
		}
		if (access_changed && !(*table_entry & PAGE_PRESENT)) {
			if (*table_entry & PAGE_FORCE) {
				*table_entry = 0;
			} else {
				if (addr < LEAST_ADDR) {
					LEAST_ADDR = addr;
				}
				if ((addr + PAGE_SIZE) > MAX_ADDR) {
					MAX_ADDR = addr + PAGE_SIZE;
				}
			}
		}
	}
	UNLOCK(&mmap_lock);
}

void kern_mmap_init (unsigned int length, unsigned int addr)
{
	uintptr_t mmap;
	DEBUG_PRINTF ("memory map at 0x%08x, length %d\r\n", addr, length);
	for (mmap = addr; mmap < (addr + length); mmap += ((multiboot_memory_map_t *)mmap)->size + sizeof (((multiboot_memory_map_t *)mmap)->size)) {
		multiboot_memory_map_t * mmm = (multiboot_memory_map_t *) mmap;
		if (mmm->type == 1 && (mmm->addr + mmm->len - 1) <= SIZE_MAX) {
			uintptr_t addr = mmm->addr;
			uintptr_t len = mmm->len;
			DEBUG_PRINTF("Available memory at 0x%08x; 0x%08x (%u) bytes\r\n", addr, len, len);
			if (addr < ONE_MB) {
				if (addr < PAGE_SIZE && addr + len >= 2 * PAGE_SIZE) {
					LOW_PAGE = PAGE_SIZE;
				} else if (addr < LOW_PAGE) {
					uintptr_t base = addr & ~(PAGE_SIZE - 1);
					if (base == addr && len >= PAGE_SIZE) {
						LOW_PAGE = base;
					} else if (addr + len >= base + 2 * PAGE_SIZE) {
						LOW_PAGE = base + PAGE_SIZE;
					}
				}
				if (addr + len <= ONE_MB) {
					DEBUG_PRINTF(" ignoring, because whole range is under 1 MB\r\n");
					continue;
				} else {
					len -= (ONE_MB - addr);
					addr = ONE_MB;
					DEBUG_PRINTF(" treating as 0x%08x, 0x%08x (%d) bytes; to avoid memory under 1 MB\r\n", addr, len, len);
				}
			}
			if (addr & (PAGE_SIZE -1)) {
				addr = (addr & ~(PAGE_SIZE -1)) + PAGE_SIZE;
				if (len >= (addr - mmm->addr)) {
					len -= (addr - mmm->addr);
				} else {
					len = 0;
				}
				DEBUG_PRINTF ("  adjusting address to page boundary, 0x%08x, 0x%08x (%d) bytes\r\n", addr, len, len);
			}
			if (len & (PAGE_SIZE -1)) {
				len &= ~(PAGE_SIZE -1);
				DEBUG_PRINTF ("  adjusting length to page boundary, 0x%08x, 0x%08x (%d) bytes\r\n", addr, len, len);
			}

			if (PAGE_DIRECTORY == 0 && len >= (PAGE_SIZE * PAGE_SIZE) + PAGE_SIZE) {
				// keep things simple, make a complete page table, with 4K sized pages
				PAGE_DIRECTORY = addr + (len - PAGE_SIZE);
				PAGE_TABLE_BASE = PAGE_DIRECTORY - (PAGE_SIZE * PAGE_SIZE);
				uintptr_t base = PAGE_TABLE_BASE;
				explicit_bzero((void *)base, PAGE_SIZE * PAGE_SIZE + PAGE_SIZE); // zero the whole structure
				add_page_mapping(PAGE_PRESENT | PAGE_READWRITE, PAGE_DIRECTORY, PAGE_DIRECTORY);
				add_page_mappings(PAGE_PRESENT | PAGE_READWRITE, base, PAGE_SIZE * PAGE_SIZE);
				len -= (PAGE_SIZE * PAGE_SIZE) + PAGE_SIZE;
			}
			if (!LEAST_ADDR || addr < LEAST_ADDR) {
				LEAST_ADDR = addr;
			}
			if (addr + len > MAX_ADDR) {
				MAX_ADDR = addr + len;
			}
			add_page_mappings(0, addr, len);

			if (mem_segment_count < MAX_MEM_SEGMENTS) {
				add_mem_segment(addr + buddy_page_data_len, len);
			} else {
				EARLY_ERROR_PRINTF("error adding memory segment to allocator: too many segments\r\n");
			}
		} else {
			uintptr_t addr = mmm->addr;
			uintptr_t len = mmm->len;
			DEBUG_PRINTF("unavailable memory (%d) at 0x%08x; 0x%08x (%u) bytes\r\n", mmm->type, addr, len, len);
		}
	}
	EARLY_ERROR_PRINTF("finished setting up pages\r\n");
	PAGE_SETUP_FINISHED = 1;
}


int kern_mmap (uintptr_t *ret, void * addr, size_t len, int prot, int flags)
{
	unsigned int alignsize = PAGE_SIZE;
	if (flags & MAP_ALIGNMENT_MASK) {
		alignsize = 1 << ((flags & MAP_ALIGNMENT_MASK) >> MAP_ALIGNMENT_SHIFT);
		if (alignsize < PAGE_SIZE) {
			alignsize = PAGE_SIZE;
		}
	}
	uint16_t mappingflags = 0;
	if (prot & PROT_READ) {
		mappingflags |= PAGE_PRESENT;
	}
	if (prot & PROT_WRITE) {
		mappingflags |= PAGE_READWRITE;
		if (! (prot & PROT_READ)) {
			halt("weird protection, write but not read\r\n", 0);
		}
	}
	if (!(prot & PROT_KERNEL)) {
		mappingflags |= PAGE_USER;
	}
	if (prot & PROT_FORCE) {
		mappingflags |= PAGE_FORCE;
	}

	if (! ((prot & PROT_KERNEL) && (flags & MAP_EARLY))) {
		LOCK(&mmap_lock);
	}
	if (len & (PAGE_SIZE -1)) {
		len = (len & ~(PAGE_SIZE - 1)) + PAGE_SIZE;
	}
	addr = (void*)((uintptr_t)addr & ~(PAGE_SIZE -1)); // align to page, just in case
	if (addr != NULL && !((prot & PROT_FORCE) || (prot & PROT_KERNEL)) && !mem_available((uintptr_t)addr, len)) {
		ERROR_PRINTF("range %08x, %08x not available; looking for anything!\r\n", addr, len);
		addr = NULL;
	} else if (addr != NULL && ((uintptr_t)addr & (alignsize - 1))) {
		ERROR_PRINTF("range %08x, %08x doesn't meet alignment size %08x; looking for anything!\r\n", addr, len, alignsize);
		addr = NULL;
	}

	DEBUG_PRINTF("looking for 0x%x bytes at %08x\r\n", len, addr);

	if (!addr) {
		int found = 0;
		if (flags & MAP_STACK) {
			*ret = (MAX_ADDR - len) & ~(alignsize - 1);
			while (*ret > LEAST_ADDR) {
				if (mem_available(*ret, len)) {
					if (*ret == MAX_ADDR - len) {
						MAX_ADDR -= len;
					}
					found = 1;
					break;
				} else if (*ret == MAX_ADDR - len && !mem_available(MAX_ADDR - PAGE_SIZE, PAGE_SIZE)) {
					MAX_ADDR -= PAGE_SIZE;
				}
				*ret -= alignsize;
			}
		} else {
			*ret = LEAST_ADDR & ~(alignsize - 1);
			while (*ret + len < MAX_ADDR) {
				if (mem_available(*ret, len)) {
					if (*ret == LEAST_ADDR) {
						LEAST_ADDR += len;
					}
					found = 1;
					break;
				} else if (*ret == LEAST_ADDR && mem_available(*ret, PAGE_SIZE)) {
					LEAST_ADDR += PAGE_SIZE;
				}
				*ret += alignsize;
			}
		}
		if (!found) {
			*ret = ENOMEM;
			ERROR_PRINTF("unable to allocate\r\n");
			ERROR_PRINTF("kern_mmap (%08x (%08x), %08x, %08x, %x, %x)\r\n", *ret, ret, addr, len, prot, flags);
			UNLOCK(&mmap_lock);
			return 0;
		}
	} else {
		*ret = (uintptr_t) addr;
	}
	add_page_mappings(mappingflags, *ret, len);
	DEBUG_PRINTF("kern_mmap (%08x (%08x), %08x, %08x, %x, %x)\r\n", *ret, ret, addr, len, prot, flags);
	if (! ((prot & PROT_KERNEL) && (flags & MAP_EARLY))) {
		UNLOCK(&mmap_lock);
	}
	return 1;
}

void kern_mmap_enable_paging() {
	asm volatile ( "mov %0, %%cr3" :: "a" (PAGE_DIRECTORY));
	uint32_t a;
	asm volatile("mov %%cr0, %0" : "=a" (a));
	a|= 0x80000001; 
	a&= 0x9FFFFFFF;
	DEBUG_PRINTF("setting cr0 to %08x\r\n", a);
	asm volatile("mov %0, %%cr0" :: "a"(a));
}

