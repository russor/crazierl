#include "/usr/src/stand/i386/libi386/multiboot.h"
#include <sys/types.h>

struct hardcoded_file {
	char* name;
	uint8_t* start;
	uint8_t* end;
	size_t size;
};

void init_files(multiboot_module_t *);
struct hardcoded_file * find_file(const char *);
struct hardcoded_file * find_dir(const char *, size_t, struct hardcoded_file *);
uintptr_t transfer_files_to_userland();
void init_files_from_userland();
