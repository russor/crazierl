#include <sys/time.h>

int gettimeofday(struct timeval *tp, struct timezone *tzp) {
	if (tp != NULL) {
	    tp->tv_sec = 0;
	    tp->tv_usec = 0;
        }
        if (tzp != NULL) {
            tzp->tz_minuteswest = 0;
            tzp->tz_dsttime = 0;
        }
        return 0;
}
