#ifndef SYS_TIME_H
#define SYS_TIME_H

#include <time.h>

struct timezone {
	int     tz_minuteswest; /*	minutes	west of	Greenwich */
	int     tz_dsttime;     /*	type of	dst correction */
};

int gettimeofday(struct timeval *tp, struct timezone *tzp);

#endif
