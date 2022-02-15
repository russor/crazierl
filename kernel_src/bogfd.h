#define BOGFD_MAX 1024
#define BNOTE_MAX (BOGFD_MAX * 4)

// enum magic from https://kubyshkin.name/posts/c-language-enums-tips-and-tricks/
#define BOGFD_TYPE_ENUM(VARIANT) \
    VARIANT(CLOSED) \
    VARIANT(TERMIN) \
    VARIANT(TERMOUT) \
    VARIANT(PIPE) \
    VARIANT(BOGFD_FILE) \
    VARIANT(DIR) \
    VARIANT(BOGFD_NULL) \
    VARIANT(IRQ) \
    VARIANT(UNIX) \
    VARIANT(KQUEUE) \
    VARIANT(PENDING)

#define BOGFD_TYPE_ENUM_VARIANT(NAME) NAME,

typedef enum {
    BOGFD_TYPE_ENUM(BOGFD_TYPE_ENUM_VARIANT)
} bogfd_type;

#define BOGFD_STATUS_SIZE sizeof(int)

struct pipe_buffer {
	ssize_t length;
	uint8_t data[];
};

#define BOGFD_PB_LEN ((PAGE_SIZE >> 1) - sizeof(struct pipe_buffer))

#define BOGFD_BLOCKED_READ  0x10000000
#define BOGFD_BLOCKED_WRITE 0x20000000


struct BogusFD {
	bogfd_type type;
	unsigned long flags;
	struct threadqueue waiters;
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
