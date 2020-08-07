RTLD=/libexec/ld-elf32.so.1
OTPDIR=../installed

KERNEL_COMPILER=clang -m32 -mno-sse -g -ffreestanding -gdwarf-2 -c -DCRAZIERL_KERNEL
USER_COMPILER=clang -m32 -fpic -g -gdwarf-2 -c -DCRAZIERL_USER
NIF_COMPILER=clang -m32 -fpic -g -gdwarf-2 -shared -I../installed/lib/erlang/usr/include/

run: obj/mykernel.elf obj/initrd
	qemu-system-i386 -smp 4 -s -m 512 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

netboot: obj/mykernel.elf obj/initrd
	scp $^ 192.168.0.12:/var/lib/tftpboot/crazierl/

debug: obj/mykernel.elf obj/initrd
	qemu-system-i386 -d cpu_reset,guest_errors -smp 2 -S -s  -m 256 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

noisy: obj/mykernel.elf obj/initrd
	qemu-system-i386 -smp 2 -d nochain,exec,cpu_reset,guest_errors -s  -m 256 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file obj/mykernel.elf" -ex "add-symbol-file $$(find $(OTPDIR) -name beam.smp)" -ex "target remote localhost:1234"

clean:
	rm -f obj/initrd obj/mykernel.elf obj/*.o obj/*.beam

obj/mykernel.elf: obj/start.o obj/kernel.o obj/syscalls.o obj/files.o obj/kern_mmap.o obj/acpi.o \
		obj/rtld_printf.o obj/bcmp.o obj/bzero.o obj/ffsl.o obj/memcpy.o obj/memmove.o \
		obj/memset.o obj/strchr.o obj/strchrnul.o obj/strcmp.o obj/strcpy.o obj/strlcpy.o \
		obj/strlen.o obj/strncmp.o obj/strncpy.o obj/strnlen.o obj/strtol.o \
		obj/qdivrem.o obj/umoddi3.o obj/udivdi3.o obj/llabs.o \
		obj/explicit_bzero.o
	clang -m32 -fuse-ld=bfd -g -static -ffreestanding -nostdlib -T linker.ld $^ -o obj/mykernel.elf -gdwarf-2

obj/start.o: start.s
	clang -m32 -g -gdwarf-2 -c $^ -o $@

obj/kernel.o: kernel.c common.h files.h kern_mmap.h bogfd.h threads.h acpi.h
	$(KERNEL_COMPILER) $< -o $@ -I /usr/src/libexec/rtld-elf/

obj/syscalls.o: /usr/src/sys/kern/syscalls.c
	$(KERNEL_COMPILER) $^ -o $@

debugnative:
	BINDIR=`pwd`/../otp_src_R12B-5/bin/ gdb $(RTLD) -ex 'break _start' -ex 'run -- -root `pwd`/../otp_src_R12B-5 -progname erl -- -home /home/toast'

obj/initrd: hardcode_files.pl preload_local_files $(shell cat preload_local_files) Makefile
	./hardcode_files.pl $(RTLD) $(OTPDIR) > obj/initrd.tmp
	mv obj/initrd.tmp obj/initrd

obj/files.o: files.c files.h
	$(KERNEL_COMPILER) $< -o $@

obj/kern_mmap.o: kern_mmap.c kern_mmap.h common.h
	$(KERNEL_COMPILER) $< -o $@

obj/acpi.o: acpi.c acpi.h
	$(KERNEL_COMPILER) -I /usr/src/sys/ $< -o $@

obj/rtld_printf.o: /usr/src/libexec/rtld-elf/rtld_printf.c
	$(KERNEL_COMPILER) $^ -o $@

obj/bcmp.o: /usr/src/lib/libc/string/bcmp.c
	$(KERNEL_COMPILER) $^ -o $@
obj/bzero.o: /usr/src/lib/libc/string/bzero.c
	$(KERNEL_COMPILER) $^ -o $@
obj/ffsl.o: /usr/src/lib/libc/string/ffsl.c
	$(KERNEL_COMPILER) $^ -o $@
obj/memcpy.o: /usr/src/lib/libc/string/memcpy.c
	$(KERNEL_COMPILER) $^ -o $@
obj/memmove.o: /usr/src/lib/libc/string/memmove.c
	$(KERNEL_COMPILER) $^ -o $@
obj/memset.o: /usr/src/lib/libc/string/memset.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strchr.o: /usr/src/lib/libc/string/strchr.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strchrnul.o: /usr/src/lib/libc/string/strchrnul.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strcmp.o: /usr/src/lib/libc/string/strcmp.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strcpy.o: /usr/src/lib/libc/string/strcpy.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strlcpy.o: /usr/src/lib/libc/string/strlcpy.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strlen.o: /usr/src/lib/libc/string/strlen.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strncmp.o: /usr/src/lib/libc/string/strncmp.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strncpy.o: /usr/src/lib/libc/string/strncpy.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strnlen.o: /usr/src/lib/libc/string/strnlen.c
	$(KERNEL_COMPILER) $^ -o $@
obj/strtol.o: strtol.c
	$(KERNEL_COMPILER) $^ -o $@

obj/qdivrem.o: /usr/src/lib/libc/quad/qdivrem.c
	$(KERNEL_COMPILER) $^ -o $@
obj/umoddi3.o: /usr/src/lib/libc/quad/umoddi3.c
	$(KERNEL_COMPILER) $^ -o $@
obj/udivdi3.o: /usr/src/lib/libc/quad/udivdi3.c
	$(KERNEL_COMPILER) $^ -o $@
obj/llabs.o: /usr/src/lib/libc/stdlib/llabs.c
	$(KERNEL_COMPILER) $^ -o $@

obj/explicit_bzero.o: /usr/src/sys/libkern/explicit_bzero.c
	$(KERNEL_COMPILER) $^ -o $@

obj/libuserland.so: obj/userland.o obj/files_userland.o
	clang -m32 -fpic -shared -Wl,-soname,libuserland.so -o obj/libuserland.so $^

obj/userland.o: userland.c
	$(USER_COMPILER) $^ -o $@

obj/files_userland.o: files.c
	$(USER_COMPILER) $^ -o $@

obj/crazierl_nif.so: crazierl_nif.c
	$(NIF_COMPILER) $^ -o $@
obj/crazierl.beam: crazierl.erl
	../installed/bin/erlc $^
	mv crazierl.beam obj/

obj/comport.beam: comport.erl
	../installed/bin/erlc $^
	mv comport.beam obj/

obj/console.beam: console.erl
	../installed/bin/erlc $^
	mv console.beam obj/

obj/pci.beam: pci.erl
	../installed/bin/erlc $^
	mv pci.beam obj/

obj/vgakb.beam: vgakb.erl
	../installed/bin/erlc $^
	mv vgakb.beam obj/

obj/acpi.beam: acpi.erl
	../installed/bin/erlc $^
	mv acpi.beam obj/
