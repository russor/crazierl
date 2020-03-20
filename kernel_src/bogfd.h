#define BOGFD_MAX_KERNEL 32 // need 4 for startup, and some number for irqs
#define BOGFD_MAX_USER 1024

#ifdef CRAZIERL_USER
#define BOGFD_MAX BOGFD_MAX_USER
#endif
#ifdef CRAZIERL_KERNEL
#define BOGFD_MAX BOGFD_MAX_KERNEL
#endif

#define BOGFD_CLOSED 0
#define BOGFD_TERMIN 1
#define BOGFD_TERMOUT 2
#define BOGFD_PIPE 3
#define BOGFD_FILE 4
#define BOGFD_DIR 5
#define BOGFD_NULL 6
#define BOGFD_KERNEL 7

#define BOGFD_STATUS_SIZE 4

struct BogusFD {
	int type;
	union {
		struct hardcoded_file * file;
		uint8_t status[BOGFD_STATUS_SIZE];
		int data;
	};
	union {
		struct BogusFD * pipe;
		uint8_t * pos;
		size_t namelen;
		char * buffer;
	};
};

