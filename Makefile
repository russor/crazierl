run: mykernel.elf
	qemu-system-i386 -m 256 -serial mon:stdio -kernel mykernel.elf -initrd ../otp_src_R12B-5/bin/i386-none-elf/beam.static

debug: mykernel.elf
	qemu-system-i386 -S -s  -m 256 -serial mon:stdio -kernel mykernel.elf -initrd ../otp_src_R12B-5/bin/i386-none-elf/beam.static

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file mykernel.elf" -ex "add-symbol-file ../otp_src_R12B-5/bin/i386-none-elf/beam.static" -ex "set confirm on" -ex "target remote localhost:1234"

run2: mykernel.elf
	qemu-system-i386 -m 256 -serial mon:stdio -kernel mykernel.elf

clean:
	rm -f mykernel.elf start.o kernel.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o linker.ld syscalls.o
	clang -g -fuse-ld=bfd --target=i386-freebsd-elf -static -ffreestanding -nostdlib -g -T linker.ld start.o kernel.o syscalls.o -o mykernel.elf -gdwarf-2 -lc

start.o: start.s
	clang -m32 -g -gdwarf-2 -c start.s -o start.o

kernel.o: kernel.c sysctl.h
	clang -m32 -mrdrnd -mno-sse -g -ffreestanding kernel.c -c -o kernel.o -gdwarf-2

syscalls.o: /usr/src/sys/kern/syscalls.c
	clang -m32 -mno-sse -g -c /usr/src/sys/kern/syscalls.c -o syscalls.o -gdwarf-2
