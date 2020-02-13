run: mykernel.elf
	qemu-system-i386 -m 256 -serial mon:stdio -kernel mykernel.elf -initrd ../otp_src_R12B-5/bin/x86_64-unknown-freebsd12.1/beam.static

debug: mykernel.elf
	qemu-system-i386 -S -s  -m 256 -serial mon:stdio -kernel mykernel.elf -initrd ../otp_src_R12B-5/bin/x86_64-unknown-freebsd12.1/beam.static

run2: mykernel.elf
	qemu-system-i386 -m 256 -serial mon:stdio -kernel mykernel.elf

clean:
	rm -f mykernel.elf start.o kernel.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o linker.ld syscalls.o
	clang -g -fuse-ld=bfd -static --target=i386-freebsd-elf -ffreestanding -nostdlib -g -T linker.ld start.o kernel.o syscalls.o -o mykernel.elf -gdwarf-2 -L ../libc/buildresults/src -lc -lprintf

start.o: start.s
	clang --target=i386-none-elf -g -gdwarf-2 -c start.s -o start.o

kernel.o: kernel.c
	clang -mno-sse -g --target=i386-none-elf -ffreestanding -nostdinc -I ../libc/include -I ../libc/printf -I ../libc/arch/x86/include -I ../extra -g -c kernel.c -o kernel.o -gdwarf-2

syscalls.o: syscalls.c
	clang -mno-sse -g --target=i386-none-elf -ffreestanding -nostdinc -I ../libc/include -I ../libc/printf -I ../libc/arch/x86/include -I ../extra -g -c syscalls.c -o syscalls.o -gdwarf-2
