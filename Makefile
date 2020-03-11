RTLD=/libexec/ld-elf32.so.1
OTPDIR=../installed

COMPILER=clang -m32 -mrdrnd -mno-sse -g -ffreestanding -gdwarf-2 -c

run: obj/mykernel.elf obj/initrd
	qemu-system-i386 -s -m 256 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

debug: obj/mykernel.elf obj/initrd
	qemu-system-i386 -d cpu_reset,guest_errors -S -s  -m 256 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file obj/mykernel.elf" -ex "add-symbol-file $$(find $(OTPDIR) -name beam) -o 0x00400000" -ex "set confirm on" -ex "target remote localhost:1234"

clean:
	rm -f obj/initrd obj/mykernel.elf obj/*.o

obj/mykernel.elf: obj/start.o obj/kernel.o obj/syscalls.o obj/files.o obj/kern_mmap.o \
		obj/rtld_printf.o obj/bzero.o obj/memcpy.o obj/memcmp.o obj/memset.o \
		obj/strchr.o obj/strchrnul.o obj/strcmp.o obj/strcpy.o obj/strlcpy.o \
		obj/strlen.o obj/strncmp.o obj/strncpy.o obj/strnlen.o \
		obj/qdivrem.o obj/umoddi3.o obj/udivdi3.o \
		obj/explicit_bzero.o
	clang -m32 -fuse-ld=bfd -g -static -ffreestanding -nostdlib -T linker.ld $^ -o obj/mykernel.elf -gdwarf-2

obj/start.o: start.s
	clang -m32 -g -gdwarf-2 -c $^ -o $@

obj/kernel.o: kernel.c files.h
	$(COMPILER) $< -o $@ -I /usr/src/libexec/rtld-elf/

obj/syscalls.o: /usr/src/sys/kern/syscalls.c
	$(COMPILER) $^ -o $@

debugnative:
	BINDIR=`pwd`/../otp_src_R12B-5/bin/ gdb $(RTLD) -ex 'break _start' -ex 'run -- -root `pwd`/../otp_src_R12B-5 -progname erl -- -home /home/toast'

obj/initrd: hardcode_files.pl preload_local_files Makefile
	./hardcode_files.pl $(RTLD) $(OTPDIR) > obj/initrd.tmp
	mv obj/initrd.tmp obj/initrd

obj/files.o: files.c files.h
	$(COMPILER) $< -o $@

obj/kern_mmap.o: kern_mmap.c kern_mmap.h
	$(COMPILER) $< -o $@

obj/rtld_printf.o: /usr/src/libexec/rtld-elf/rtld_printf.c
	$(COMPILER) $^ -o $@

obj/bzero.o: /usr/src/lib/libc/string/bzero.c
	$(COMPILER) $^ -o $@
obj/memcpy.o: /usr/src/lib/libc/string/memcpy.c
	$(COMPILER) $^ -o $@
obj/memcmp.o: /usr/src/lib/libc/string/memcmp.c
	$(COMPILER) $^ -o $@
obj/memset.o: /usr/src/lib/libc/string/memset.c
	$(COMPILER) $^ -o $@
obj/strchr.o: /usr/src/lib/libc/string/strchr.c
	$(COMPILER) $^ -o $@
obj/strchrnul.o: /usr/src/lib/libc/string/strchrnul.c
	$(COMPILER) $^ -o $@
obj/strcmp.o: /usr/src/lib/libc/string/strcmp.c
	$(COMPILER) $^ -o $@
obj/strcpy.o: /usr/src/lib/libc/string/strcpy.c
	$(COMPILER) $^ -o $@
obj/strlcpy.o: /usr/src/lib/libc/string/strlcpy.c
	$(COMPILER) $^ -o $@
obj/strlen.o: /usr/src/lib/libc/string/strlen.c
	$(COMPILER) $^ -o $@
obj/strncmp.o: /usr/src/lib/libc/string/strncmp.c
	$(COMPILER) $^ -o $@
obj/strncpy.o: /usr/src/lib/libc/string/strncpy.c
	$(COMPILER) $^ -o $@
obj/strnlen.o: /usr/src/lib/libc/string/strnlen.c
	$(COMPILER) $^ -o $@

obj/qdivrem.o: /usr/src/lib/libc/quad/qdivrem.c
	$(COMPILER) $^ -o $@
obj/umoddi3.o: /usr/src/lib/libc/quad/umoddi3.c
	$(COMPILER) $^ -o $@
obj/udivdi3.o: /usr/src/lib/libc/quad/udivdi3.c
	$(COMPILER) $^ -o $@

obj/explicit_bzero.o: /usr/src/sys/libkern/explicit_bzero.c
	$(COMPILER) $^ -o $@
