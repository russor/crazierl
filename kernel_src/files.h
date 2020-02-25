#include <sys/types.h>

struct hardcoded_file {
	char* name;
	void* start;
	void* end;
	size_t size;
};

void init_files();
struct hardcoded_file * find_file(const char *);
