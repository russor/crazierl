
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <term.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/rtprio.h>
#include <sys/umtx.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <poll.h>
#include <sys/mount.h>
#include <sys/dirent.h>
#include <netdb.h>
#include "files.h"
#include "bogfd.h"
#include "common.h"
#include "/usr/src/lib/libc/include/libc_private.h"

struct BogusFD FDS[BOGFD_MAX];
size_t next_fd;


void setup_fds() 
{
	FDS[0].type = BOGFD_KERNEL;
	FDS[0].data = 0;
	FDS[1].type = BOGFD_KERNEL;
	FDS[1].data = 1;
	FDS[2].type = BOGFD_KERNEL;
	FDS[2].data = 2;
	next_fd = 3;
	for (int i = next_fd; i < BOGFD_MAX; ++i) {
		FDS[i].type = BOGFD_CLOSED;
	}
}

__attribute__ ((constructor))
void init_userland () {
	setup_fds();
	init_files_from_userland();
	
	printf("hi from userland\n");
}

void _exit(int status) {
	printf("exit %d\n", status);
	while (1) { }
}

ssize_t readlink(const char *restrict path, char *restrict buf, size_t bufsiz)
{
	errno = ENOENT;
	return -1;
}

__asm__(".global  _ioctl\n _ioctl = ioctl");
int ioctl(int fd, unsigned long request, ...)
{
	va_list ap;
	va_start(ap, request);
	DEBUG_PRINTF("ioctl (%d, %08lx, ...)\n", fd, request);
	int ret = -1;
	switch (request) {
		case TIOCGETA: {
			struct termios *t = va_arg(ap, struct termios *);
			if (fd >= BOGFD_MAX || (FDS[fd].type != BOGFD_KERNEL)) {
				errno = ENOTTY;
				break;
			}
			t->c_iflag = 11010;
			t->c_oflag = 3;
			t->c_cflag = 19200;
			t->c_lflag = 1483;
			//t->c_cc = "\004\377\377\177\027\025\022\b\003\034\032\031\021\023\026\017\001\000\024\377";
			t->c_ispeed = 38400;
			t->c_ospeed = 38400;
			ret = 0;
			break;
		}
		case TIOCSETA: {
			struct termios *t = va_arg(ap, struct termios *);
			ret = 0; // ignore settings
			break;
		}
		case TIOCGWINSZ: {
			struct winsize *w = va_arg(ap, struct winsize *);
			if (fd >= BOGFD_MAX || (FDS[fd].type != BOGFD_KERNEL)) {
				errno = ENOTTY;
				break;
			}
			w->ws_row = 25;
			w->ws_col = 80;
			ret = 0;
			break;
		}
		default:
			errno = ENOTTY;
			char *ignore = va_arg(ap, char *);
	}
	va_end(ap);
	DEBUG_PRINTF("fd %d, parm_len %ld, cmd %ld, group %c\n", fd, IOCPARM_LEN(request), request & 0xff, (char) IOCGROUP(request));
	return ret;
}

int rtprio_thread(int function, lwpid_t lwpid, struct rtprio *rtp)
{
	if (function == RTP_LOOKUP) {
		rtp->type = RTP_PRIO_NORMAL;

		return 0;
	}
	ERROR_PRINTF("rtprio_thread(%d, %d, ...)\n", function, lwpid);
	exit(EINVAL);
}

__asm__(".global  _open\n _open = open");
int open (const char *p, int flags, ...)
{
	while (next_fd < BOGFD_MAX && FDS[next_fd].type != BOGFD_CLOSED) {
		++next_fd;
	}
	if (next_fd >= BOGFD_MAX) {
		ERROR_PRINTF("open (%s) = EMFILE\n", p);
		errno = EMFILE;
		return -1;
	}

	char path[256];
	strlcpy (path, p, sizeof(path));
	if (strcmp(path, ".") == 0) {
		strcpy(path, "");
	}

	struct hardcoded_file * file;
	if (flags & O_DIRECTORY) {
		size_t len = strlen(path);
		file = find_dir(path, len, 0);
		if (file) {
			FDS[next_fd].type = BOGFD_DIR;
			FDS[next_fd].file = file;
			FDS[next_fd].namelen = len;
			DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
			return next_fd++;
		}
	} else {
		file = find_file(path);
		if (file != NULL) {
			FDS[next_fd].type = BOGFD_FILE;
			FDS[next_fd].file = file;
			FDS[next_fd].pos = file->start;
			DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
			return next_fd++;
		}
	}
	if (strcmp("/dev/null", path) == 0) {
		FDS[next_fd].type = BOGFD_NULL;
		DEBUG_PRINTF("open (%s, ...) = %d\n", path, next_fd);
		return next_fd++;
	}
	if (strncmp("/kern", path, 5) == 0) {
		int fd = __sys_open(path, flags);
		if (fd == -1) { return -1; }
		FDS[next_fd].type = BOGFD_KERNEL;
		FDS[next_fd].data = fd;
		return next_fd++;
	}

	DEBUG_PRINTF ("open (%s, %08x) = ENOENT\n", path, flags);
	errno = ENOENT;
	return -1;
}
int openat (int fd, const char *path, int flags, ...)
{
	return open(path, flags);
}

int pipe2(int fildes[2], int flags)
{
	DEBUG_PRINTF("pipe2 (%p, %08x)\n", fildes, flags);
	FDS[next_fd].type = BOGFD_PIPE;
	FDS[next_fd + 1].type = BOGFD_PIPE;
	FDS[next_fd].pipe = &(FDS[next_fd + 1]);
	FDS[next_fd + 1].pipe = &(FDS[next_fd]);
	fildes[0] = next_fd;
	fildes[1] = next_fd + 1;
	next_fd += 2;
	return 0;
}

int pipe(int filedes[2])
{
	return pipe2(filedes, 0);
}

__asm__(".global  _close\n _close = close");
int close(int fd)
{
	if (FDS[fd].type != BOGFD_CLOSED) {
		DEBUG_PRINTF("close (%d)\n", fd);
		if (FDS[fd].type == BOGFD_KERNEL) {
			__sys_close(FDS[fd].data);
		}
		FDS[fd].type = BOGFD_CLOSED;
		FDS[fd].file = NULL;
		FDS[fd].buffer = NULL;
		if (fd <= next_fd) {
			next_fd = fd;
		}
		return 0;
	} else {
		errno = EBADF;
		return -1;
	}
}

int fcntl (int fd, int cmd, ...)
{
	ERROR_PRINTF("fcntl (%d, %08x, ...)\n", fd, cmd);
	return 0;
}

int access (const char *path, int mode)
{
	DEBUG_PRINTF("access (%s, %d)\n", path, mode);
	errno = ENOENT;
	return -1;
}

int socketpair (int domain, int type, int protocol, int *sv)
{
	DEBUG_PRINTF("socketpair(%d, %d, %d, %p)\n", domain, type, protocol, sv);
	return pipe(sv);
}

int socket (int domain, int type, int protocol)
{
	ERROR_PRINTF("socket(%d, %d, %d)\n", domain, type, protocol);
	errno = EACCES;
	return -1;
}
int getsockopt(int s, int level, int optname, void * restrict optval,
               socklen_t * restrict optlen)
{
	errno = EBADF;
	return -1;
}

pid_t fork()
{
	errno = EAGAIN;
	return -1;
}

int __getcwd(char *buf, size_t size)
{
	if (size >= 2) {
		strlcpy(buf, "/", size);
		return 0;
	}
	errno = EINVAL;
	return -1;
}

__asm__(".global  _fstat\n _fstat = fstat");
int fstat (int fd, struct stat *sb)
{
	if (fd < BOGFD_MAX && (FDS[fd].type == BOGFD_TERMIN || FDS[fd].type == BOGFD_TERMOUT)) {
		explicit_bzero(sb, sizeof(*sb));
		sb->st_mode = S_IWUSR | S_IRUSR | S_IFCHR;
		return 0;
	} else if (fd < BOGFD_MAX && FDS[fd].type == BOGFD_FILE) {
		explicit_bzero(sb, sizeof(*sb));
		sb->st_dev = BOGFD_FILE;
		sb->st_ino = (ino_t) FDS[fd].file;
		sb->st_nlink = 1;
		sb->st_size = FDS[fd].file->size;
		sb->st_mode = S_IRUSR | S_IFREG | S_IRWXU;
		return 0;
	}
	errno = EBADF;
	return -1;
}

__asm__(".global  _fstatfs\n _fstatfs = fstatfs");
int fstatfs(int fd, struct statfs *buf) {
	if (fd < BOGFD_MAX && (FDS[fd].type == BOGFD_DIR || FDS[fd].type == BOGFD_FILE)) {
		bzero(buf, sizeof(*buf));
		buf->f_version = STATFS_VERSION;
		strlcpy(buf->f_fstypename, "BogusFS", sizeof(buf->f_fstypename));
		return 0;
	}
	errno = EBADF;
	return -1;
}


int stat(const char *path, struct stat *sb)
{
	DEBUG_PRINTF("fstatat (%s, %p)\n", path, sb);
	struct hardcoded_file * file;
	file = find_file(path);
	if (file != NULL) {
		explicit_bzero(sb, sizeof(*sb));
		sb->st_dev = BOGFD_FILE;
		sb->st_ino = (ino_t) file;
		sb->st_nlink = 1;
		sb->st_size = file->size;
		sb->st_mode = S_IRUSR | S_IFREG;
		return 0;
	}
	errno = ENOENT;
	return -1;
}

ssize_t pread(int fd, void *buf, size_t nbytes, off_t offset)
{
	DEBUG_PRINTF("pread (%d, %p, %d, %lld)\n", fd, buf, nbytes, offset);
	if (fd < BOGFD_MAX && FDS[fd].type == BOGFD_FILE) {
		if (offset < 0) {
			errno = EINVAL;
			return -1;
		}
		ssize_t read = min(nbytes, FDS[fd].file->size - offset);
		memcpy(buf, FDS[fd].file->start + offset, read);
		return read;
	}
	ERROR_PRINTF("pread (%d, %p, %d, %lld) = EBADF\n", fd, buf, nbytes, offset);
	errno = EBADF;
	return -1;
}

__asm__(".global  _read\n _read = read");
ssize_t read(int fd, void *buf, size_t nbytes)
{
	if (fd < 0 || fd >= BOGFD_MAX) {
		ERROR_PRINTF("write (%d, %p, %d) = EBADF\n", fd, buf, nbytes);
		errno = EBADF;
		return -1;
	}
	if (FDS[fd].type == BOGFD_FILE) {
		DEBUG_PRINTF("read (%d, %p, %d)\n", fd, buf, nbytes);
		int read = min(nbytes, FDS[fd].file->end - FDS[fd].pos);
		memcpy(buf, FDS[fd].pos, read);
		FDS[fd].pos += read;
		return read;
	} else if (FDS[fd].type == BOGFD_KERNEL) {
		return __sys_read(FDS[fd].data, buf, nbytes);
	}
	ERROR_PRINTF("read (%d, %p, %d) = EBADF\n", fd, buf, nbytes);
	errno = EBADF;
	return -1;
}

__asm__(".global  _write\n _write = write");
ssize_t write(int fd, const void *buf, size_t nbytes)
{
	if (fd < 0 || fd >= BOGFD_MAX) {
		ERROR_PRINTF("write (%d, %p, %d) = EBADF\n", fd, buf, nbytes);
		errno = EBADF;
		return -1;
	}
	if (FDS[fd].type == BOGFD_KERNEL) {
		return __sys_write(FDS[fd].data, buf, nbytes);
	} else if (FDS[fd].type == BOGFD_PIPE) {
		ERROR_PRINTF("write (%d, %p, %d) to pipe\n", fd, buf, nbytes);
		int written = 0;
		const uint8_t *mybuf = buf;
		while (written < nbytes && FDS[fd].pipe->status[0] < BOGFD_STATUS_SIZE) {
			FDS[fd].pipe->status[1 + FDS[fd].pipe->status[0]] = *mybuf;
			++FDS[fd].pipe->status[0];
			++mybuf;
		}
		if (written) {
			return written;
		} else {
			errno = EAGAIN;
			return -1;
		}
	}
	ERROR_PRINTF("write (%d, %p, %d) = EBADF\n", fd, buf, nbytes);
	errno = EBADF;
	return -1;
}

int ppoll(struct pollfd fds[], nfds_t nfds, const struct timespec * restrict timeout,
          const sigset_t * restrict newsigmask)
{
	int waitleft = timeout->tv_sec;
	int printed = 0;
	int changedfds = 0;
	struct pollfd kern_fds[nfds];

	for (int i = 0; i < nfds; ++i) {
		kern_fds[i].fd = -1;
		kern_fds[i].events = 0;
		kern_fds[i].revents = 0;
		if (fds[i].fd < 0 || fds[i].fd >= BOGFD_MAX) { continue; } // no EBADF?
		struct BogusFD *fd = &FDS[fds[i].fd];
		if (fds[i].events & POLLIN && fd->type == BOGFD_PIPE && fd->status[0] != 0) {
			ERROR_PRINTF("fd %d ready to read\n", fds[i].fd);
			fds[i].revents = POLLIN;
			++changedfds;
		} else if (fd->type == BOGFD_KERNEL) {
			kern_fds[i].fd = fd->data;
			kern_fds[i].events = fds[i].events;
		}
	}
	int kernelfds;
	if (changedfds) {
		struct timespec zero = { 0, 0 }; 
		kernelfds = __sys_ppoll(kern_fds, nfds, &zero, newsigmask);
	} else {
		kernelfds = __sys_ppoll(kern_fds, nfds, timeout, newsigmask);
	}
	if (kernelfds) {
		for (int i = 0; i < nfds; ++i) {
			if (kern_fds[i].fd != -1) {
				fds[i].revents = kern_fds[i].revents;
			}
		}
	}

	DEBUG_PRINTF("ppoll for %d fds, timeout %d\n", nfds, timeout->tv_sec);
	for (int i = 0; i < nfds; ++i) {
		DEBUG_PRINTF("  FD %d: events %08x, revents %08x\n", fds[i].fd, fds[i].events, fds[i].revents);
	}
	return changedfds + kernelfds;
}

__asm__(".global  _getdirentries\n _getdirentries = getdirentries");
ssize_t getdirentries(int fd, char *buf, size_t nbytes, off_t *basep) {
	if (fd < BOGFD_MAX && FDS[fd].type == BOGFD_DIR) {
		struct dirent *b = (struct dirent*) buf;
		if (basep != NULL) {
			*basep = (off_t) FDS[fd].file;
		}
		if (FDS[fd].file == NULL) {
			return 0;
		}
		bzero(b, sizeof(*b));
		b->d_fileno = (ino_t) FDS[fd].file;
		b->d_reclen = sizeof(*b);
		char * start = FDS[fd].file->name + FDS[fd].namelen + 1;
		char * nextslash = strchr(start, '/');
		
		if (nextslash != NULL) {
			b->d_type = DT_DIR;
			b->d_namlen = nextslash - start;
			strlcpy(b->d_name, start, b->d_namlen + 1);
			struct hardcoded_file * file = FDS[fd].file;
			while (FDS[fd].file != NULL && strncmp(
					FDS[fd].file->name + FDS[fd].namelen + 1,
					file->name + FDS[fd].namelen + 1,
					b->d_namlen + 1) == 0) {
				file = FDS[fd].file;
				FDS[fd].file = find_dir(file->name, FDS[fd].namelen, file + 1);
			}
		} else {
			b->d_type = DT_REG;
			strlcpy(b->d_name, start, sizeof(b->d_name));
			b->d_namlen = strlen(b->d_name);
			FDS[fd].file = find_dir(FDS[fd].file->name, FDS[fd].namelen, FDS[fd].file + 1);
			
		}
		b->d_off = (off_t) FDS[fd].file;
		return (b->d_reclen);
	}
	ERROR_PRINTF("getdirentries (%d)\n", fd);
	errno = EBADF;
	return -1;
}
