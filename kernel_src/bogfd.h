#define BOGFD_MAX 1024

#define BOGFD_CLOSED 0
#define BOGFD_TERMIN 1
#define BOGFD_TERMOUT 2
#define BOGFD_PIPE 3
#define BOGFD_FILE 4
#define BOGFD_DIR 5
#define BOGFD_NULL 6
#define BOGFD_KERNEL 7
#define BOGFD_PIC1 8
#define BOGFD_UNIX 9

#define BOGFD_STATUS_SIZE sizeof(int)

struct pipe_buffer {
	ssize_t length;
	uint8_t data[];
};

#define BOGFD_PB_LEN ((PAGE_SIZE >> 1) - sizeof(struct pipe_buffer))


struct BogusFD {
	int type;
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

