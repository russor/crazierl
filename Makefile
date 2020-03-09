RTLD=/libexec/ld-elf32.so.1
OTPDIR=../installed

COMPILER=clang -m32 -mrdrnd -mno-sse -g -ffreestanding -gdwarf-2 -c

run: mykernel.elf initrd
	qemu-system-i386 -s -m 256 -serial mon:stdio -kernel mykernel.elf -append $(RTLD) -initrd initrd

debug: mykernel.elf initrd
	qemu-system-i386 -d cpu_reset,guest_errors -S -s  -m 256 -serial mon:stdio -kernel mykernel.elf -append $(RTLD) -initrd initrd

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file mykernel.elf" -ex "add-symbol-file $$(find $(OTPDIR) -name beam) -o 0x00400000" -ex "set confirm on" -ex "target remote localhost:1234"

clean:
	rm -f initrd mykernel.elf *.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o syscalls.o files.o kern_mmap.o rtld_printf.o \
		bzero.o memcpy.o memcmp.o memset.o strchr.o strchrnul.o strcmp.o strcpy.o strlcpy.o strlen.o strncmp.o strncpy.o strnlen.o \
		qdivrem.o umoddi3.o udivdi3.o \
		explicit_bzero.o
	clang -m32 -fuse-ld=bfd -g -static -ffreestanding -nostdlib -T linker.ld $^ -o mykernel.elf -gdwarf-2

start.o: start.s
	clang -m32 -g -gdwarf-2 -c $^ -o $@

kernel.o: kernel.c files.h
	$(COMPILER) $< -o $@ -I /usr/src/libexec/rtld-elf/

syscalls.o: /usr/src/sys/kern/syscalls.c
	$(COMPILER) $^ -o $@

debugnative:
	BINDIR=`pwd`/../otp_src_R12B-5/bin/ gdb $(RTLD) -ex 'break _start' -ex 'run -- -root `pwd`/../otp_src_R12B-5 -progname erl -- -home /home/toast'

initrd: hardcode_files.pl preload_local_files Makefile
	./hardcode_files.pl $(RTLD) $(OTPDIR) > initrd.tmp
	mv initrd.tmp initrd

files.o: files.c files.h
	$(COMPILER) $< -o $@

kern_mmap.o: kern_mmap.c kern_mmap.h
	$(COMPILER) $< -o $@

rtld_printf.o: /usr/src/libexec/rtld-elf/rtld_printf.c
	$(COMPILER) $^ -o $@

bzero.o: /usr/src/lib/libc/string/bzero.c
	$(COMPILER) $^ -o $@
memcpy.o: /usr/src/lib/libc/string/memcpy.c
	$(COMPILER) $^ -o $@
memcmp.o: /usr/src/lib/libc/string/memcmp.c
	$(COMPILER) $^ -o $@
memset.o: /usr/src/lib/libc/string/memset.c
	$(COMPILER) $^ -o $@
strchr.o: /usr/src/lib/libc/string/strchr.c
	$(COMPILER) $^ -o $@
strchrnul.o: /usr/src/lib/libc/string/strchrnul.c
	$(COMPILER) $^ -o $@
strcmp.o: /usr/src/lib/libc/string/strcmp.c
	$(COMPILER) $^ -o $@
strcpy.o: /usr/src/lib/libc/string/strcpy.c
	$(COMPILER) $^ -o $@
strlcpy.o: /usr/src/lib/libc/string/strlcpy.c
	$(COMPILER) $^ -o $@
strlen.o: /usr/src/lib/libc/string/strlen.c
	$(COMPILER) $^ -o $@
strncmp.o: /usr/src/lib/libc/string/strncmp.c
	$(COMPILER) $^ -o $@
strncpy.o: /usr/src/lib/libc/string/strncpy.c
	$(COMPILER) $^ -o $@
strnlen.o: /usr/src/lib/libc/string/strnlen.c
	$(COMPILER) $^ -o $@

qdivrem.o: /usr/src/lib/libc/quad/qdivrem.c
	$(COMPILER) $^ -o $@
umoddi3.o: /usr/src/lib/libc/quad/umoddi3.c
	$(COMPILER) $^ -o $@
udivdi3.o: /usr/src/lib/libc/quad/udivdi3.c
	$(COMPILER) $^ -o $@

explicit_bzero.o: /usr/src/sys/libkern/explicit_bzero.c
	$(COMPILER) $^ -o $@
