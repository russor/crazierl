run: mykernel.elf
	qemu-system-i386 -serial mon:stdio -kernel mykernel.elf

clean:
	rm -f mykernel.elf start.o kernel.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o linker.ld
	clang -g -fuse-ld=bfd -static --target=i386-freebsd-elf -ffreestanding -nostdlib -g -T linker.ld start.o kernel.o -o mykernel.elf -gdwarf-2 -L ../libc/buildresults/src -lc -lprintf

start.o:
	clang --target=i386-none-elf -c start.s -o start.o

kernel.o:
	clang -mno-sse -g --target=i386-none-elf -ffreestanding -nostdinc -I ../libc/include -I ../libc/printf -I ../libc/arch/x86/include -g -c kernel.c -o kernel.o -gdwarf-2
