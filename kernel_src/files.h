#include <sys/types.h>

struct hardcoded_file {
	char* name;
	void* start;
	void* end;
	size_t size;
};

void init_files();
struct hardcoded_file * find_file(const char *);
struct hardcoded_file * find_dir(const char *, size_t, struct hardcoded_file *);
