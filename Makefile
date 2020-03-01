#BEAM=../otp_src_R12B-5/bin/i386-none-elf/beam.static
BEAM=/libexec/ld-elf32.so.1

run: mykernel.elf initrd
	qemu-system-i386 -s -m 256 -serial mon:stdio -kernel mykernel.elf -append $(BEAM) -initrd initrd

debug: mykernel.elf initrd
	qemu-system-i386 -S -s  -m 256 -serial mon:stdio -kernel mykernel.elf -append $(BEAM) -initrd initrd

debugger:
	gdb -ex "add-symbol-file mykernel.elf"  -ex "set confirm off" -ex "target remote localhost:1234"

clean:
	rm -f mykernel.elf start.o kernel.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o printf.o linker.ld syscalls.o files.o
	clang -g -fuse-ld=bfd --target=i386-freebsd-elf -static -ffreestanding -nostdlib -g -T linker.ld start.o kernel.o printf.o syscalls.o files.o -o mykernel.elf -gdwarf-2 -lc

start.o: start.s
	clang -m32 -g -gdwarf-2 -c start.s -o start.o

kernel.o: kernel.c files.h
	clang -m32 -mrdrnd -mno-sse -g -ffreestanding kernel.c -c -o kernel.o -gdwarf-2 -I ../libc/printf

syscalls.o: /usr/src/sys/kern/syscalls.c
	clang -m32 -mno-sse -g -c /usr/src/sys/kern/syscalls.c -o syscalls.o -gdwarf-2

printf.o: ../libc/printf/printf.c
	clang -m32 -mno-sse -g -c ../libc/printf/printf.c -o printf.o -gdwarf-2

debugnative:
	BINDIR=`pwd`/../otp_src_R12B-5/bin/ gdb $(BEAM) -ex 'break _start' -ex 'run -- -root `pwd`/../otp_src_R12B-5 -progname erl -- -home /home/toast'

initrd: hardcode_files.pl preload_local_files preload_otp_files Makefile
	./hardcode_files.pl $(BEAM) > initrd.tmp
	mv initrd.tmp initrd

files.o: files.c files.h
	clang -m32 -mno-sse -g -c files.c -o files.o -gdwarf-2
