
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/rtprio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include "common.h"

__attribute__ ((constructor))
void init_userland () {
	printf("hi from userland\n");
}

ssize_t readlink(const char *restrict path, char *restrict buf, size_t bufsiz)
{
	errno = ENOENT;
	return -1;
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

