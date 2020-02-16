/*
 * Top-level identifiers
 */
#define	CTL_UNSPEC	0		/* unused */
#define	CTL_KERN	1		/* "high kernel": proc, limits */
#define	CTL_VM		2		/* virtual memory */
#define	CTL_VFS		3		/* filesystem, mount type is next */
#define	CTL_NET		4		/* network, see socket.h */
#define	CTL_DEBUG	5		/* debugging parameters */
#define	CTL_HW		6		/* generic cpu/io */
#define	CTL_MACHDEP	7		/* machine dependent */
#define	CTL_USER	8		/* user-level */
#define	CTL_P1003_1B	9		/* POSIX 1003.1B */

#define	KERN_OSRELDATE		24	/* int: kernel release date */
#define	KERN_ARND		37	/* int: from arc4rand() */

#define VM_OVERCOMMIT           12      /* vm.overcommit */


#define	HW_NCPU		 3		/* int: number of cpus */
#define	HW_PAGESIZE	 7		/* int: software page size */

#define	CTL_P1003_1B_PAGESIZE			20	/* int */

