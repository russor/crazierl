#define BOGFD_MAX 1024

#define BOGFD_CLOSED 0
#define BOGFD_TERMIN 1
#define BOGFD_TERMOUT 2
#define BOGFD_PIPE 3
#define BOGFD_FILE 4
#define BOGFD_DIR 5
#define BOGFD_NULL 6
#define BOGFD_IOAPIC 7
#define BOGFD_UNIX 8
#define BOGFD_KQUEUE 9
#define BOGFD_STATUS_SIZE sizeof(int)

struct pipe_buffer {
	ssize_t length;
	uint8_t data[];
};

#define BOGFD_PB_LEN ((PAGE_SIZE >> 1) - sizeof(struct pipe_buffer))

#define BOGFD_BLOCKED_READ  0x10000000
#define BOGFD_BLOCKED_WRITE 0x20000000


struct BogusFD {
	int type;
	unsigned long flags;
	DECLARE_LOCK(lock);
	union {
		struct hardcoded_file * file;
		struct pipe_buffer * pb;
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

