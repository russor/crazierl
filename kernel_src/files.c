#include "common.h"
#include "files.h"
#include "kern_mmap.h"
#include <string.h>
//#include <lz4.h>

#define MAX_FILES 256
struct hardcoded_file hardcoded_files[MAX_FILES];

uint32_t unpack_network(uintptr_t start) {
	return *(uint8_t*)(start + 3) | (*(uint8_t*)(start + 2) << 8)
		| (*(uint8_t*)(start + 1) << 16) | (*(uint8_t*)(start) << 24);
}

void init_files(multiboot_module_t *mod) {
	uintptr_t scratch;
	kern_mmap(&scratch, mod, sizeof (*mod), PROT_READ | PROT_KERNEL | PROT_FORCE, 0);
	uintptr_t start = mod->mod_start;
	if (mod->mod_end - start < 8) {
		ERROR_PRINTF("initrd is too small to be useful %d\n", mod->mod_end - start);
		return;
	}
	kern_mmap(&scratch, (void *)start, mod->mod_end - start, PROT_READ | PROT_KERNEL | PROT_FORCE, 0);
	uint32_t files = unpack_network(start);
	if (files > MAX_FILES) {
		ERROR_PRINTF("too many files in initrd %d > %d\n", files, MAX_FILES);
		return;
	}
	start += sizeof(uint32_t);

	uint32_t total_namelen = unpack_network(start);
	start += sizeof(uint32_t);
	uintptr_t names; 
	if (!kern_mmap(&names, NULL, total_namelen, PROT_READ | PROT_WRITE | PROT_KERNEL, MAP_ANON | MAP_STACK)) {
		ERROR_PRINTF("couldn't allocate %d bytes for filenames\n", total_namelen);
		return;
	}
	
	for (int i = 0; start < mod->mod_end; ++i) {
		size_t strlen = strnlen((char *)start, mod->mod_end - start) + 1;
		if (strlen + start + sizeof(uint32_t) > mod->mod_end) { break; }
		size_t filelen = unpack_network(start + strlen);
		size_t compressedlen = 0;
		if (filelen & 0x80000000) {
			if (strlen + start + sizeof(uint32_t) * 2 > mod->mod_end) { break; }
			compressedlen = unpack_network(start + strlen + sizeof(uint32_t));
			filelen &= 0x7FFFFFFF;
		}

		if (compressedlen) {
			if (strlen + start + sizeof(uint32_t) * 2 + compressedlen > mod->mod_end) { break; }
		} else {
			if (strlen + start + sizeof(uint32_t) + filelen > mod->mod_end) { break; }
		}

		uintptr_t file;
		size_t mmaplen = filelen;
		if (mmaplen & (PAGE_SIZE -1)) {
			mmaplen = (mmaplen & ~(PAGE_SIZE -1)) + PAGE_SIZE;
		}

		if (!kern_mmap(&file, NULL, mmaplen, PROT_READ | PROT_WRITE | PROT_KERNEL, MAP_ANON | MAP_STACK)) {
			ERROR_PRINTF("couldn't allocate %d bytes (%d) for file %s\n", mmaplen, filelen, (char *)start);
			return;
		}

		memcpy((uint8_t*)names, (uint8_t*)start, strlen);
		hardcoded_files[i].name = (char *) names;
		names += strlen;
		
		hardcoded_files[i].end = (uint8_t *) (file + filelen);
		start += strlen + sizeof(uint32_t);

		if (compressedlen) {
			start += sizeof(uint32_t);
			//if (LZ4_decompress_safe((char *)start, (char *)file, compressedlen, filelen) != filelen) {
				ERROR_PRINTF("couldn't decompress %s\n", hardcoded_files[i].name);
				return;
			//}
		} else {
			memcpy((uint8_t *)file, (uint8_t*)start, filelen);
		}

		if (mmaplen > filelen) {
			explicit_bzero((uint8_t *) (file + filelen), mmaplen - filelen);
		}
		hardcoded_files[i].start = (uint8_t *) file;		
		hardcoded_files[i].size = filelen;
		start += filelen;
	}
	kern_munmap(PROT_KERNEL, mod->mod_start, mod->mod_end - mod->mod_start);
}

struct hardcoded_file * find_file(const char * name) {
	for (int i = 0; i < MAX_FILES; ++i) {
		if (hardcoded_files[i].name == NULL) { continue; }
		int cmp = strcmp(name, hardcoded_files[i].name);
		if (cmp == 0) {
			return &(hardcoded_files[i]);
		} else if (cmp < 0) {
			return NULL;
		}
	} 
	return NULL;
}

struct hardcoded_file * find_dir(const char * name, size_t len, struct hardcoded_file *i) {
	if (i == NULL) {
		i = &hardcoded_files[0];
	}
	for (; i <= &hardcoded_files[MAX_FILES -1] && i->name != NULL; ++i) {
		if (i->name == NULL) { continue; }
		int cmp = strncmp(name, i->name, len);
		if (cmp == 0) {
			if (i->name[len] == '/') {
				return i;
			}
		} else if (cmp < 0) {
			return NULL;
		}
	} 
	return NULL;
}
