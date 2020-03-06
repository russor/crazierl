RTLD=/libexec/ld-elf32.so.1
OTPDIR=../installed

COMPILER=clang -m32 -mrdrnd -mno-sse -g -ffreestanding -gdwarf-2 -c

run: mykernel.elf initrd
	qemu-system-i386 -s -m 256 -serial mon:stdio -kernel mykernel.elf -append $(RTLD) -initrd initrd

debug: mykernel.elf initrd
	qemu-system-i386 -d cpu_reset,guest_errors -S -s  -m 256 -serial mon:stdio -kernel mykernel.elf -append $(RTLD) -initrd initrd

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file mykernel.elf" -ex "set confirm on" -ex "target remote localhost:1234"

clean:
	rm -f initrd mykernel.elf *.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o linker.ld syscalls.o files.o kern_mmap.o
	clang -g -fuse-ld=bfd --target=i386-freebsd-elf -static -ffreestanding -nostdlib -g -T linker.ld start.o kernel.o syscalls.o files.o kern_mmap.o -o mykernel.elf -gdwarf-2 -lc

start.o: start.s
	clang -m32 -g -gdwarf-2 -c start.s -o start.o

kernel.o: kernel.c files.h
	$(COMPILER) kernel.c -o kernel.o

syscalls.o: /usr/src/sys/kern/syscalls.c
	$(COMPILER) /usr/src/sys/kern/syscalls.c -o syscalls.o

printf.o: ../libc/printf/printf.c
	$(COMPILER) ../libc/printf/printf.c -o printf.o

debugnative:
	BINDIR=`pwd`/../otp_src_R12B-5/bin/ gdb $(RTLD) -ex 'break _start' -ex 'run -- -root `pwd`/../otp_src_R12B-5 -progname erl -- -home /home/toast'

initrd: hardcode_files.pl preload_local_files Makefile
	./hardcode_files.pl $(RTLD) $(OTPDIR) > initrd.tmp
	mv initrd.tmp initrd

files.o: files.c files.h
	$(COMPILER) files.c -o files.o

kern_mmap.o: kern_mmap.c kern_mmap.h
	$(COMPILER) kern_mmap.c -o kern_mmap.o
