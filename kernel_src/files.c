#include "files.h"
#include <string.h>

#define MAX_FILES 256
struct hardcoded_file hardcoded_files[MAX_FILES];

void init_files(multiboot_module_t *mod, uintptr_t *max_addr) {
	uintptr_t start = mod->mod_start;
	for (int i = 0; start < mod->mod_end; ++i) {
		size_t strlen = strnlen((char *)start, mod->mod_end - start) + 1;
		if (strlen + start + sizeof(uint32_t) > mod->mod_end) { break; }
		size_t filelen = *(uint8_t*)(start + strlen + 3) | (*(uint8_t*)(start + strlen + 2) << 8)
			 | (*(uint8_t*)(start + strlen + 1) << 16) | (*(uint8_t*)(start + strlen) << 24);
		if (strlen + start + sizeof(uint32_t) + filelen > mod->mod_end) { break; }
		*max_addr -= strlen;
		memcpy((uint8_t*)*max_addr, (uint8_t*)start, strlen);
		hardcoded_files[i].name = (char *) *max_addr;
		hardcoded_files[i].end = (uint8_t *) *max_addr;
		*max_addr -= filelen + sizeof(uint32_t);
		start += strlen + sizeof(uint32_t);
		memcpy((uint8_t *)*max_addr, (uint8_t*)start, filelen);
		hardcoded_files[i].start = (uint8_t *) *max_addr;		
		hardcoded_files[i].size = filelen;
		start += filelen;
	}
};

struct hardcoded_file * find_file(const char * name) {
	for (int i = 0; i < MAX_FILES; ++i) {
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
