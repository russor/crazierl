#define BOGFD_MAX 1024
#define BNOTE_MAX (BOGFD_MAX * 4)

#define BOGFD_CLOSED 0
#define BOGFD_TERMIN 1
#define BOGFD_TERMOUT 2
#define BOGFD_PIPE 3
#define BOGFD_FILE 4
#define BOGFD_DIR 5
#define BOGFD_NULL 6
#define BOGFD_IRQ 7
#define BOGFD_UNIX 8
#define BOGFD_KQUEUE 9
#define BOGFD_PENDING 10
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
	struct lock lock;
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
	size_t bnote;
	
};

struct bnote { // like a FreeBSD knote, but Bogus
	struct lock lock;
	size_t link; // list of knotes for a kqueue
	size_t selnext; // list of knotes for a watched object
	short filter;
	unsigned short flags;
	unsigned int fflags;
	__int64_t data;
	void *udata;
	int status;
	size_t kq; // FD index of the kqueue
	size_t fd; // FD index of the FD (which is all we support for now)
};

// real FreeBSD filters are all negative
#define BNOTE_PENDING 1
