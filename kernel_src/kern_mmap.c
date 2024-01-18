#include "common.h"
#include "kern_mmap.h"

#include "/usr/src/stand/i386/libi386/multiboot.h"
#include <stdint.h>
#include <strings.h>
#include <errno.h>
#include <sys/queue.h>
#include <stdlib.h>
#include <strings.h>

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
#define PAGE_BUDDY 0x800

#define MAX_MEM_SEGMENTS 128
#define MAX_PAGE_ORDER 12
#define PAGE_ORDER_MASK 0x7F
#define FREELIST_COUNT MAX_PAGE_ORDER + 1

uintptr_t PAGE_DIRECTORY;
uintptr_t PAGE_TABLE_BASE;
uintptr_t LEAST_ADDR;
uintptr_t MAX_ADDR;
uintptr_t LOW_PAGE = 0;
int PAGE_SETUP_FINISHED;

struct mem_segment {
	uintptr_t addr;
	size_t len;
	size_t start_page;
};

struct page {
	uint8_t order;
	uint8_t free;
	uint8_t segment1;
	LIST_ENTRY(page) freelist;
};

struct page *PAGE_DATA;
struct mem_segment mem_segments[MAX_MEM_SEGMENTS];
int mem_segment_count;

LIST_HEAD(freelist, page) freelists[FREELIST_COUNT];

uint32_t * pagetable_direntry (uintptr_t logical)
{
	return (uint32_t *) (PAGE_DIRECTORY + ((logical >> PAGE_DIRECTORY_BITS) * sizeof(uint32_t)));
}

uint32_t * pagetable_entry (uint32_t direntry, uintptr_t logical)
{
	uintptr_t lookup = PAGE_FLOOR(direntry) | (((logical >> PAGE_LEVEL_BITS) & PAGE_LEVEL_MASK) * sizeof(uint32_t));
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

void buddy_unfree(uintptr_t);

void add_page_mapping (uint16_t flags, uintptr_t logical, uintptr_t physical) {
	uint32_t * directory_entry = pagetable_direntry(logical);
	if (*directory_entry == 0) {
		if (unlikely(!PAGE_SETUP_FINISHED || (flags & PAGE_FORCE))) {
			*directory_entry = (PAGE_TABLE_BASE + ((logical >> PAGE_DIRECTORY_BITS) * PAGE_SIZE)) | (flags & (PAGE_SIZE -1));
		} else {
			EARLY_ERROR_PRINTF("add_page_mapping (0x%x, %p, %p)\r\n", flags, logical, physical);
			halt("Trying to setup a mapping without a directory entry 1\r\n", 0);
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
			*table_entry = PAGE_FLOOR(physical) | (flags & (PAGE_SIZE - 1));
		} else {
			EARLY_ERROR_PRINTF("add_page_mapping (0x%x, %p, %p)\r\n", flags, logical, physical);
			EARLY_ERROR_PRINTF("table_entry %p (%x), directory_entry %p (%x), logical %p\r\n",
				table_entry, *table_entry, directory_entry, *directory_entry, logical);
			halt("Trying to setup a mapping without a page table entry 2\r\n", 0);
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
		if (!(flags & PAGE_FORCE) && !(flags & PAGE_BUDDY)) {
			buddy_unfree(physical);
		}
	}
}

void add_page_mappings (uint16_t mappingflags, uintptr_t addr, size_t len) {
	while (len) {
		add_page_mapping(mappingflags, addr, addr);
		addr += PAGE_SIZE;
		len -= PAGE_SIZE;
	}
}

struct page *get_segment_page(struct mem_segment *segment, uintptr_t addr) {
	if (segment == NULL) {
		return NULL;
	}
	if (addr >= segment->addr && addr + PAGE_SIZE <= segment->addr + segment->len) {
		return &PAGE_DATA[segment->start_page + (addr - segment->addr) / PAGE_SIZE];
	}
	return NULL;
}

uintptr_t get_page_addr(struct mem_segment *segment, struct page *page) {
	return (page - &PAGE_DATA[segment->start_page]) * PAGE_SIZE + segment->addr;
}

struct mem_segment *get_page_segment(struct page *page) {
	if (page->segment1 > 0) {
		return &mem_segments[page->segment1 - 1];
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

uint8_t buddy_order(size_t len) {
	uint8_t order = flsl(len);
	if (ffsl(len) == order) {
		order -=1;
	}
	order -= 12;
	return order;
}

struct page *get_area_buddy(struct page *page, uint8_t order) {
	struct mem_segment *segment = get_page_segment(page);
	uintptr_t page_addr = get_page_addr(segment, page);
	uintptr_t buddy_addr = page_addr ^ (PAGE_SIZE << (order & PAGE_ORDER_MASK));
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
		struct page *free_area_buddy_page = get_area_buddy(free_area_first_page, order);

                free_area_first_page->order = order;
                free_area_buddy_page->order = order;
                free_area_buddy_page->free = 1;

		// add the resultant areas of the split to the freelist
		LIST_INSERT_HEAD(&freelists[order], free_area_first_page, freelist);
		LIST_INSERT_HEAD(&freelists[order], free_area_buddy_page, freelist);
	}

	// we are guaranteed at least two free areas on the target freelist now
	struct page *free_area = LIST_FIRST(&freelists[target_order]);
	return free_area;
}


void buddy_free(struct page *area) {
	struct mem_segment *segment = get_page_segment(area);
	if (segment == NULL) {
		uintptr_t struct_page_addr = (uintptr_t) area;
		EARLY_ERROR_PRINTF("invalid free of %08x outside memory segment\r\n", struct_page_addr);
		halt("invalid free outside memory segment\r\n", 0);
	}
	uint8_t order = 0;
	struct page *p = area;
	while (p->free != 1 && p->order != order) {
		EARLY_ERROR_PRINTF("buddy free loop %p order %d\r\n", get_page_addr(segment, p), p->order);
		order = p->order;
		p = get_area_buddy(p, p->order - 1); // note p->order can't be zero
	}
	if (p->free) {
		uintptr_t addr = get_page_addr(segment, area);
		uintptr_t addr2 = get_page_addr(segment, p);
		EARLY_ERROR_PRINTF("double free of %p (%p order %d)\r\n", addr, addr2, p->order);
		halt("double free\r\n", 0);
        } else if (p->order > 0) {
		uintptr_t addr = get_page_addr(segment, area);
		uintptr_t addr2 = get_page_addr(segment, p);

		EARLY_ERROR_PRINTF("buddy free of %p (%p order %d)\r\n", addr, addr2, p->order);
		halt("fixme\r\n", 0);
        }

	// mark the area as free and put on freelist
	area->free = 1;
	LIST_INSERT_HEAD(&freelists[area->order], area, freelist);

	// merge with other areas as much as possible
	while (area->order < MAX_PAGE_ORDER) {
		struct page *buddy = get_area_buddy(area, area->order);
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
		area = first_area;

		// add the new merged area to the right freelist
		LIST_INSERT_HEAD(&freelists[area->order], first_area, freelist);
	}
}

void buddy_free_addr(uintptr_t addr) {
	struct mem_segment *s = get_addr_mem_segment(addr);
	if (s == NULL) {
		halt("no segment\r\n", 0);
	}
	struct page *p = get_segment_page(s, addr);
	if (p == NULL) {
		halt("no page\r\n", 0);
	}
	//ERROR_PRINTF("buddy free %p\r\n", addr);
	buddy_free(p);
}

void buddy_unfree(uintptr_t addr) {
	struct mem_segment *s = get_addr_mem_segment(addr);
	if (s == NULL) {
		halt("no segment\r\n", 0);
	}
	struct page *p = get_segment_page(s, addr);
	if (p == NULL) {
		halt("no page\r\n", 0);
	}
	uint8_t order = 0;
	while (p->free != 1 && p->order != order) {
		order = p->order;
		p = get_area_buddy(p, p->order - 1); // note p->order can't be zero
	}
	if (p->free == 1) {
		order = p->order;
		LIST_REMOVE(p, freelist);
		while (order) {
			order -= 1;
			struct page *b = get_area_buddy(p, order);
			p->order = order;
			b->order = order;
			b->free = 1;
			//EARLY_ERROR_PRINTF("free %x (%x) order %d\r\n", addr, get_page_addr(s, b), b->order);

			if (addr >= get_page_addr(s, b)) {
				//EARLY_ERROR_PRINTF("unfree split order %d, %p\r\n", order, get_page_addr(get_page_segment(p), p));
				LIST_INSERT_HEAD(&freelists[order], p, freelist);
				p = b;
			} else {
				//EARLY_ERROR_PRINTF("unfree split order %d, %p\r\n", order, get_page_addr(get_page_segment(b), b));
				LIST_INSERT_HEAD(&freelists[order], b, freelist);
			}
		}
		//EARLY_ERROR_PRINTF("unfree %p (%p)\r\n", addr, get_page_addr(s, p));
		p->free = 0;
	} else {
		EARLY_ERROR_PRINTF("unfree %p %p (%p)\r\n", addr, p, get_page_addr(s, p));
		halt("not free in buddy_unfree\r\n", 0);
	}
}

void add_mem_segment(size_t segment) {
	// initialize the beginning of the segment for struct pages
	struct mem_segment * s = &mem_segments[segment];
	if (segment > 0) {
		s->start_page = mem_segments[segment - 1].start_page + (mem_segments[segment - 1].len / PAGE_SIZE);
	}

	uintptr_t page_count = s->len / PAGE_SIZE;

	struct page *page, *start;
	start = &PAGE_DATA[s->start_page];
	for (page = start; page < start + page_count; page++) {
		page->segment1 = segment + 1;
		buddy_free(page);
	}
}

DECLARE_LOCK(mmap_lock);

void kern_munmap (uint16_t mode, uintptr_t addr, size_t size)
{
	addr = PAGE_FLOOR(addr);
	size = PAGE_CEIL(size);
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
				buddy_free_addr(addr);
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

int comp_addr (const void* a, const void* b) {
	return ((struct mem_segment*)a)->addr - ((struct mem_segment*)b)->addr;
}

void kern_mmap_init (unsigned int length, unsigned int addr, uintptr_t max_used_addr)
{
	uintptr_t mmap;
	size_t pages = 0;
	EARLY_ERROR_PRINTF ("memory map at 0x%08x, length %d\r\n", addr, length);
	for (mmap = addr; mmap < (addr + length); mmap += ((multiboot_memory_map_t *)mmap)->size + sizeof (((multiboot_memory_map_t *)mmap)->size)) {
		multiboot_memory_map_t * mmm = (multiboot_memory_map_t *) mmap;
		if (mmm->type == 1 && (mmm->addr + mmm->len - 1) <= SIZE_MAX) {
			uintptr_t addr = mmm->addr;
			uintptr_t len = mmm->len;
			EARLY_ERROR_PRINTF("Available memory at 0x%08x-0x%08x; 0x%08x (%u) bytes\r\n", addr, addr + len - 1, len, len);
			addr = PAGE_FLOOR(addr);
			if (addr != mmm->addr) {
				addr += PAGE_SIZE;
				len -= (addr - mmm->addr);
			}

			len = PAGE_FLOOR(len);

			if (addr == 0) {
				len -= PAGE_SIZE;
				addr = PAGE_SIZE;
			}

			if (addr < ONE_MB && LOW_PAGE == 0 && len >= PAGE_SIZE) {
				LOW_PAGE = addr;
				addr += PAGE_SIZE;
				len -= PAGE_SIZE;
			}
			if (len == 0) {
				continue;
			}

			if (mem_segment_count < MAX_MEM_SEGMENTS) {
				struct mem_segment *segment = &mem_segments[mem_segment_count];
				segment->addr = addr;
				segment->len = len;

				mem_segment_count += 1;
				pages += len / PAGE_SIZE;
			} else {
				EARLY_ERROR_PRINTF("error adding memory segment to allocator: too many segments\r\n");
			}
		} else {
			uintptr_t addr = mmm->addr;
			uintptr_t len = mmm->len;
			EARLY_ERROR_PRINTF("unavailable memory (%d) at 0x%08x; 0x%08x (%u) bytes\r\n", mmm->type, addr, len, len);
		}
	}

	// sort, just in case!
	qsort(mem_segments, mem_segment_count, sizeof(struct mem_segment), comp_addr);
	for (int i = 0; i < mem_segment_count; ++i) {
		if (i + 1 < mem_segment_count && mem_segments[i].addr + mem_segments[i].len > mem_segments[i + 1].addr) {
			EARLY_ERROR_PRINTF("mem segment %d %p %d bytes overlaps mem segment %p %d bytes\r\n",
			                   i, mem_segments[i].addr, mem_segments[i].len,
			                   mem_segments[i + 1].addr, mem_segments[i + 1].len);
			halt("bad memory map\r\n", 0);
		}
	}

	for (int i = mem_segment_count - 1; i >= 0; --i) {
		struct mem_segment *segment = &mem_segments[i];
		if (segment->len >= (PAGE_SIZE * PAGE_SIZE) + PAGE_SIZE) {
			// keep things simple, make a complete page table, with 4K sized pages
			PAGE_DIRECTORY = segment->addr + (segment->len - PAGE_SIZE);
			PAGE_TABLE_BASE = PAGE_DIRECTORY - (PAGE_SIZE * PAGE_SIZE);
			uintptr_t base = PAGE_TABLE_BASE;
			if (base > max_used_addr) {
				explicit_bzero((void *)base, PAGE_SIZE * PAGE_SIZE + PAGE_SIZE); // zero the whole structure
				add_page_mapping(PAGE_PRESENT | PAGE_READWRITE, PAGE_DIRECTORY, PAGE_DIRECTORY);
				add_page_mappings(PAGE_PRESENT | PAGE_READWRITE, base, PAGE_SIZE * PAGE_SIZE);
				segment->len -= (PAGE_SIZE * PAGE_SIZE) + PAGE_SIZE;
				pages -= (PAGE_SIZE + 1); // page directory is one page, page table is PAGE_SIZE pages
				break;
			} else {
				PAGE_DIRECTORY = PAGE_TABLE_BASE = 0;
			}
		}
	}
	if (PAGE_DIRECTORY == 0) {
		halt("couldn't find room for page table\r\n", 1);
	}

        size_t page_data_byte_size = PAGE_CEIL(sizeof(struct page) * pages);
	for (int i = mem_segment_count - 1; i >= 0; --i) {
		struct mem_segment *segment = &mem_segments[i];
		if (segment->len >= page_data_byte_size) {
			PAGE_DATA = (struct page *)(segment->addr + segment->len - page_data_byte_size);
			if ((uintptr_t)PAGE_DATA > max_used_addr) {
				add_page_mappings(PAGE_PRESENT | PAGE_READWRITE, (uintptr_t)PAGE_DATA, page_data_byte_size);
				explicit_bzero(PAGE_DATA, page_data_byte_size);
				segment->len -= page_data_byte_size;
				break;
			} else {
				PAGE_DATA = NULL;
			}
		}
	}
	if (PAGE_DATA == NULL) {
		halt("couldn't find room for page data\r\n", 0);
	}

	for (int i = 0; i < mem_segment_count; ++i) {
		struct mem_segment *segment = &mem_segments[i];
		if (!LEAST_ADDR || segment->addr < LEAST_ADDR) {
			LEAST_ADDR = segment->addr;
		}
		if (segment->addr + segment->len > MAX_ADDR) {
			MAX_ADDR = segment->addr + segment->len;
		}
		EARLY_ERROR_PRINTF("add mem segment %x %x (%u)\r\n", segment->addr, segment->len, segment->len);
		add_page_mappings(0, segment->addr, segment->len);
		add_mem_segment(i);
	}

	EARLY_ERROR_PRINTF("finished setting up pages\r\n");
	PAGE_SETUP_FINISHED = 1;
}

int mem_available (uintptr_t addr, size_t len)
{
	uintptr_t a = addr;
	size_t l = len;
	while (len) {
		uint32_t * page_table_entry = pagetable_entry(*(pagetable_direntry(addr)), addr);
		if (*page_table_entry == 0 || (*page_table_entry & PAGE_PRESENT)) {
			return 0;
		}
		addr += PAGE_SIZE;
		len -= PAGE_SIZE;
	}
	// REDO check with buddy allocator
	addr = a;
	len = l;
	while (len > 0) {
		struct mem_segment *s = get_addr_mem_segment(addr);
		if (s == NULL) {
			halt("no segment\r\n", 0);
			return 0;
		}
		struct page *p = get_segment_page(s, addr);
		if (p == NULL) {
			halt("no page\r\n", 0);
			return 0;
		}
		uint8_t order = 0;
		while (p->free != 1 && p->order != order) {
			order = p->order;
			p = get_area_buddy(p, p->order - 1); // note p->order can't be zero
		}
		if (p->free == 1) {
			uintptr_t endaddr = get_page_addr(s, p) + (PAGE_SIZE << (p->order));
			if (endaddr > addr) {
				len = 0;
			} else {
				len -= (endaddr - addr);
			}
			addr = endaddr;
		} else {
			ERROR_PRINTF("addr %x, len %x (%d)\r\n", addr, len, len);
			ERROR_PRINTF("p addr %x, free %d, order %d\r\n", get_page_addr(s, p), p->free, p->order);
			halt("not free in mem_available\r\n", 0);
			return 0;
		}
	}
	return 1;
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
	len = PAGE_CEIL(len);
	addr = (void*)PAGE_FLOOR(addr);
	if (addr != NULL && !((prot & PROT_FORCE) || (flags & MAP_FIXED)) && !mem_available((uintptr_t)addr, len)) {
		ERROR_PRINTF("range %08x, %08x not available; looking for anything!\r\n", addr, len);
		//halt("couldn't satisfy range\r\n", 0);
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
			uint8_t order = buddy_order(max(len, alignsize));
			struct page *page = buddy_alloc(order);
			if (page != NULL) {
				struct mem_segment *segment = get_page_segment(page);
				*ret = get_page_addr(segment, page);
				found = 1;
			}
		}
		if (!found) {
			*ret = ENOMEM;
			ERROR_PRINTF("unable to allocate\r\n");
			ERROR_PRINTF("kern_mmap (%08x (%08x), %08x, %08x, %x, %x)\r\n", *ret, ret, addr, len, prot, flags);
			if (! ((prot & PROT_KERNEL) && (flags & MAP_EARLY))) {
				UNLOCK(&mmap_lock);
			}
			return 0;
		}
	} else {
		*ret = (uintptr_t) addr;
	}
	if ((*ret & (alignsize - 1)) != 0) {
		halt("bad alignment\r\n", 0);
	}

	DEBUG_PRINTF("kern_mmap (%08x-%08x, %08x, %08x, %x, %x)\r\n", *ret, (*ret + len -1), addr, len, prot, flags);
	add_page_mappings(mappingflags, *ret, len);
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

