ERLANG_VERSION=24

OBJDIR := obj
DEPDIR := $(OBJDIR)/.deps
DEPFLAGS = -MT $@ -MMD -MP

ERLANG_OVERRIDES = $(wildcard overrides/*.erl)

ERLANG_SRCS = $(wildcard *.erl)
ERLANG_OBJS = $(ERLANG_SRCS:%.erl=$(OBJDIR)/%.beam)
ERLANG_OVERRIDE_OBJS = $(ERLANG_OVERRIDES:overrides/%.erl=$(OBJDIR)/%.beam)
INITRD_ERLANG_OBJS = $(filter-out $(OBJDIR)/hook_module.beam,$(ERLANG_OBJS)) $(ERLANG_OVERRIDE_OBJS)
KERNEL_SRCS = kernel.c files.c kern_mmap.c acpi.c strtol.c rand.c
KERNEL_OBJS = $(KERNEL_SRCS:%.c=$(OBJDIR)/%.o)

REAL_FBSD_KERNEL_SRCS = lib/libc/quad/qdivrem.c lib/libc/quad/udivdi3.c \
                   lib/libc/quad/umoddi3.c lib/libc/stdlib/llabs.c \
                   lib/libc/string/bcmp.c lib/libc/string/bzero.c \
                   lib/libc/string/ffsl.c lib/libc/string/memcpy.c \
                   lib/libc/string/memmove.c lib/libc/string/memset.c \
                   lib/libc/string/strchr.c lib/libc/string/strchrnul.c \
                   lib/libc/string/strcmp.c lib/libc/string/strcpy.c \
                   lib/libc/string/strlcpy.c lib/libc/string/strlen.c \
                   lib/libc/string/strncmp.c lib/libc/string/strncpy.c \
                   lib/libc/string/strnlen.c libexec/rtld-elf/rtld_printf.c \
                   sys/kern/syscalls.c sys/libkern/explicit_bzero.c
FBSD_KERNEL_SRCS = $(foreach file, $(REAL_FBSD_KERNEL_SRCS), $(subst /,__,$(file)))
FBSD_KERNEL_OBJS = $(FBSD_KERNEL_SRCS:%.c=$(OBJDIR)/%.o)

REAL_BEARSSL_SRCS = rand/hmac_drbg.c mac/hmac.c hash/sha2small.c codec/dec32be.c \
                    codec/enc32be.c
BEARSSL_SRCS = $(foreach file, $(REAL_BEARSSL_SRCS), $(subst /,__,$(file)))
BEARSSL_OBJS = $(BEARSSL_SRCS:%.c=$(OBJDIR)/%.o)



USER_SRCS = userland.c files.c
USER_OBJS = $(USER_SRCS:%.c=$(OBJDIR)/user.%.o)

TCPIP_SRCS = $(filter-out %eth_port.erl,$(wildcard ../erlang-tcpip/src/*.erl))
TCPIP_OBJS = $(TCPIP_SRCS:../erlang-tcpip/src/%.erl=$(OBJDIR)/%.beam)

ifeq ($(wildcard /libexec/ld-elf32.so.1),)
	RTLD=/libexec/ld-elf.so.1
else
	RTLD=/libexec/ld-elf32.so.1
endif
#OTPDIR=../installed/lib/erlang
OTPDIR=../erlang-runtime$(ERLANG_VERSION)/usr/local/lib/erlang$(ERLANG_VERSION)

KERNEL_COMPILER=clang -m32 -mno-sse -g -ffreestanding -gdwarf-2 -c -DCRAZIERL_KERNEL
USER_COMPILER=clang -m32 -fpic -g -gdwarf-2 -c -DCRAZIERL_USER
NIF_COMPILER=clang -m32 -fpic -g -gdwarf-2 -shared -I$(OTPDIR)/usr/include/


run: obj/mykernel.elf obj/initrd
	qemu-system-i386 --no-reboot -display none -smp 1 -s -m 512 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd \
		-netdev user,id=mynet0,hostfwd=tcp:127.0.0.1:7780-:80,hostfwd=tcp:127.0.0.1:7781-:8080 -device virtio-net,netdev=mynet0 -object filter-dump,id=mynet0,netdev=mynet0,file=/tmp/crazierl.pcap

netboot: obj/mykernel.elf obj/initrd
	cp $^ /usr/local/www/apache24/data/tftpboot/crazierl/

debug: obj/mykernel.elf obj/initrd
	qemu-system-i386 -display none -d cpu_reset,guest_errors -smp 2 -S -s  -m 256 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

noisy: obj/mykernel.elf obj/initrd
	qemu-system-i386 -display none -smp 2 -d nochain,exec,cpu_reset,guest_errors -s  -m 256 -serial mon:stdio -kernel obj/mykernel.elf -append $(RTLD) -initrd obj/initrd

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file obj/mykernel.elf" -ex "add-symbol-file $$(find $(OTPDIR) -name beam.smp)" -ex "target remote localhost:1234"

.PHONY: clean $(OTPDIR)/bin/erlc
clean:
	rm -f obj/initrd obj/mykernel.elf obj/*.o obj/*.beam obj/*.so obj/initrd.tmp obj/.deps/*.d

$(OTPDIR)/bin/erl:
	mkdir -p ../erlang-runtime$(ERLANG_VERSION)
	INSTALL_AS_USER=1 pkg --root ../erlang-runtime$(ERLANG_VERSION) -o ABI=FreeBSD:13:i386 install -y erlang-runtime$(ERLANG_VERSION)


$(OTPDIR)/bin/erl.patched: $(OTPDIR)/bin/erl
	sed -e 's@"/usr/local/lib/erlang$(ERLANG_VERSION)"@"$(shell pwd)/$(OTPDIR)"@' -i backup $(OTPDIR)/bin/erl
	touch $(OTPDIR)/bin/erl.patched

$(OTPDIR)/bin/erlc: $(OTPDIR)/bin/erl.patched

ALL_KERNEL_OBJS = $(KERNEL_OBJS) $(FBSD_KERNEL_OBJS) $(BEARSSL_OBJS) obj/start.o
$(OBJDIR)/mykernel.elf: $(ALL_KERNEL_OBJS) linker.ld
	clang -m32 -g -static -ffreestanding -nostdlib -Xlinker -Tlinker.ld -Xlinker $(ALL_KERNEL_OBJS)  -o obj/mykernel.elf -gdwarf-2

obj/start.o: start.s | $(DEPDIR)
	clang -m32 -g -gdwarf-2 -c $^ -o $@

INITRD_FILES := .erlang.cookie cfg/inetrc obj/etcpip.app /usr/share/misc/termcap.db obj/libuserland.so obj/crazierl_nif.so obj/checksum.so $(TCPIP_OBJS) $(INITRD_ERLANG_OBJS)

.erlang.cookie: gen_cookie.escript $(OTPDIR)/bin/escript
	$(OTPDIR)/bin/escript gen_cookie.escript > .erlang.cookie.tmp
	mv .erlang.cookie.tmp .erlang.cookie

obj/initrd: hardcode_files.pl extract_start.escript $(OTPDIR)/bin/escript $(INITRD_FILES) Makefile
	./hardcode_files.pl $(RTLD) $(OTPDIR) \
		OTPDIR/lib/crypto-*/ebin/crypto.beam OTPDIR/lib/crypto-*/priv/lib/crypto.so OTPDIR/lib/crypto-*/priv/lib/crypto_callback.so \
		$(INITRD_FILES) > obj/initrd.tmp
	mv obj/initrd.tmp obj/initrd

obj/libuserland.so: $(USER_OBJS)
	clang -m32 -fpic -shared -Wl,-soname,libuserland.so -o obj/libuserland.so $^

obj/crazierl_nif.so: crazierl_nif.c $(OTPDIR)/bin/erl
	$(NIF_COMPILER) $< -o $@

obj/checksum.so: ../erlang-tcpip/c_src/checksum.c
	$(NIF_COMPILER) $< -o $@

obj/etcpip.app: etcpip.app
	cp $< $@

$(TCPIP_OBJS): $(OBJDIR)/%.beam : ../erlang-tcpip/src/%.erl $(DEPDIR)/%.d | $(DEPDIR) $(OTPDIR)/bin/erlc
	$(OTPDIR)/bin/erlc -o $(OBJDIR)/ -MMD -MF $(DEPDIR)/$*.d $<

$(ERLANG_OBJS): $(OBJDIR)/%.beam : %.erl $(DEPDIR)/%.d | $(DEPDIR) $(OTPDIR)/bin/erlc
	$(OTPDIR)/bin/erlc -o $(OBJDIR)/ -MMD -MF $(DEPDIR)/$*.d $<

$(ERLANG_OVERRIDE_OBJS): $(OBJDIR)/%.beam : overrides/%.erl $(DEPDIR)/%.d $(OBJDIR)/hook_module.beam | $(DEPDIR) $(OTPDIR)/bin/erlc
	$(OTPDIR)/bin/erlc -I $(OTPDIR)/lib/kernel-*/include -pz $(shell pwd)/$(OBJDIR)/ -o $(OBJDIR)/ -MMD -MF $(DEPDIR)/$*.d '+{parse_transform,hook_module}' $<

$(KERNEL_OBJS): $(OBJDIR)/%.o: %.c $(DEPDIR)/%.c.d | $(DEPDIR)
	$(KERNEL_COMPILER) $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $< -I /usr/src/libexec/rtld-elf/ -I /usr/src/sys/ -I /usr/src/contrib/bearssl/inc/ -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

$(FBSD_KERNEL_OBJS): $(OBJDIR)/%.o: $(DEPDIR)/%.c.d | $(DEPDIR)
	$(KERNEL_COMPILER) -I. $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $(subst __,/,/usr/src/$*.c) -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

$(BEARSSL_OBJS): $(OBJDIR)/%.o: $(DEPDIR)/%.c.d | $(DEPDIR)
	$(KERNEL_COMPILER) -I /usr/src/contrib/bearssl/src/ -I /usr/src/contrib/bearssl/inc/ $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $(subst __,/,/usr/src/contrib/bearssl/src/$*.c) -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

$(USER_OBJS) : $(OBJDIR)/user.%.o: %.c $(DEPDIR)/user.%.c.d | $(DEPDIR)
	$(USER_COMPILER) $(DEPFLAGS)  -MF $(DEPDIR)/user.$*.c.d.T $< -o $@
	mv -f $(DEPDIR)/user.$*.c.d.T $(DEPDIR)/user.$*.c.d && touch $@

$(DEPDIR): ; @mkdir -p $@
DEPFILES := $(ERLANG_OBJS:$(OBJDIR)/%.beam=$(DEPDIR)/%.d) $(ERLANG_OVERRIDE_OBJS:$(OBJDIR)/%.beam=$(DEPDIR)/%.d) $(TCPIP_OBJS:$(OBJDIR)/%.beam=$(DEPDIR)/%.d) $(KERNEL_SRCS:%.c=$(DEPDIR)/%.c.d) $(FBSD_KERNEL_SRCS:%.c=$(DEPDIR)/%.c.d) $(BEARSSL_SRCS:%.c=$(DEPDIR)/%.c.d) $(USER_SRCS:%.c=$(DEPDIR)/user.%.c.d) $(NIF_SRCS:%.c=$(DEPDIR)/nif.%.d)
$(DEPFILES):
include $(wildcard $(DEPFILES))
