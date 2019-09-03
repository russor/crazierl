run: mykernel.elf
	qemu-system-i386 -serial mon:stdio -kernel mykernel.elf

clean:
	rm -f mykernel.elf start.o kernel.o

# use wrong target for linking, so clang does the linking instead of calling into gcc
mykernel.elf: start.o kernel.o linker.ld
	clang --target=i386-freebsd-elf -ffreestanding -nostdlib -g -T linker.ld start.o kernel.o -o mykernel.elf

start.o:
	clang --target=i386-none-elf -ffreestanding -g -c start.s -o start.o

kernel.o:
	clang --target=i386-none-elf -ffreestanding -g -c kernel.c -o kernel.o
