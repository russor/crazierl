ERLANG_VERSION=26
#OTPDIR=../installed/lib/erlang
OTPDIR=erlang-runtime$(ERLANG_VERSION)/usr/local/lib/erlang$(ERLANG_VERSION)

OBJDIR := obj
DEPDIR := $(OBJDIR)/.deps
DEPFLAGS = -MT $@ -MMD -MP
HWNODE ?= 'crazierl@crazierlp.ruka.org'
ROOTDIR := 

ERLANG_OVERRIDES = $(wildcard overrides/*.erl)

ERLANG_SRCS = $(wildcard src/*.erl)
ERLANG_OBJS = $(ERLANG_SRCS:src/%.erl=$(OBJDIR)/%.beam)
ERLANG_OVERRIDE_OBJS = $(ERLANG_OVERRIDES:overrides/%.erl=$(OBJDIR)/%.beam)
INITRD_ERLANG_OBJS = $(filter-out $(OBJDIR)/hook_module.beam,$(ERLANG_OBJS)) $(ERLANG_OVERRIDE_OBJS)
KERNEL_SRCS = $(wildcard kernel_src/*.c)
KERNEL_OBJS = $(KERNEL_SRCS:kernel_src/%.c=$(OBJDIR)/%.o)
NIF_SRCS = $(wildcard c_src/*_nif.c)
NIF_OBJS = $(NIF_SRCS:c_src/%.c=$(OBJDIR)/%.so)


REAL_FBSD_KERNEL_SRCS = lib/libc/quad/qdivrem.c lib/libc/quad/udivdi3.c \
                   lib/libc/quad/umoddi3.c lib/libc/quad/divdi3.c \
                   lib/libc/stdlib/llabs.c lib/libc/stdlib/qsort.c \
                   lib/libc/string/bcmp.c lib/libc/string/bzero.c \
                   lib/libc/string/ffsl.c lib/libc/string/flsl.c \
                   lib/libc/string/memcpy.c \
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

TCPIP_SRCS = $(filter-out %eth_port.erl,$(wildcard erlang-tcpip/src/*.erl))
TCPIP_OBJS = $(TCPIP_SRCS:erlang-tcpip/src/%.erl=$(OBJDIR)/%.beam)

ifeq ($(wildcard $(ROOTDIR)/libexec/ld-elf32.so.1),)
	RTLD=$(ROOTDIR)/libexec/ld-elf.so.1
else
	RTLD=$(ROOTDIR)/libexec/ld-elf32.so.1
endif

KERNEL_COMPILER=clang -Werror -m32 -mno-sse -g -ffreestanding -gdwarf-2 -mgeneral-regs-only -mno-red-zone -c -DCRAZIERL_KERNEL
NIF_COMPILER=clang -Werror -m32 -fpic -g -gdwarf-2 -shared -I$(OTPDIR)/usr/include/ -I kernel_src/

run: obj/crazierl.elf obj/initrd
	qemu-system-i386 -cpu max --no-reboot -display none -smp 16 -s -m 512 -serial mon:stdio -kernel obj/crazierl.elf -append $(RTLD) -initrd obj/initrd \
		-netdev user,hostname=localhost,id=mynet0,hostfwd=tcp:127.0.0.1:7780-:80,hostfwd=tcp:127.0.0.1:7781-:8080,hostfwd=tcp:127.0.0.1:4370-:4370 -device virtio-net,netdev=mynet0 -object filter-dump,id=mynet0,netdev=mynet0,file=/tmp/crazierl.pcap

build: obj/crazierl.elf obj/initrd
	echo "Built"

obj/crazierl.elf.gz: obj/crazierl.elf
	gzip -f -9 -k $^

obj/initrd.gz: obj/initrd
	gzip -f -9 -k $^

obj/crazierl.iso: obj/initrd.gz obj/crazierl.elf.gz cfg/grub.cfg
	mkdir -p obj/iso/boot/grub
	cp obj/initrd.gz obj/crazierl.elf.gz obj/iso
	cp cfg/grub.cfg obj/iso/boot/grub
	grub-mkrescue -o obj/crazierl.iso obj/iso/

# if I can figure out how to get iPXE to use gz files...
#netboot: obj/crazierl.elf.gz obj/initrd.gz
netboot: obj/crazierl.elf obj/initrd
	cp $^ /usr/local/www/apache24/data/tftpboot/crazierl/

push-to-demo: obj/crazierl.elf obj/initrd
	rsync $^ dh1.ruka.org:crazierl/

iso: obj/crazierl.iso
	cp $^ /usr/local/www/apache24/data/tftpboot/crazierl/
	rsync $^ dh1.ruka.org:crazierl/

debug: obj/crazierl.elf obj/initrd
	qemu-system-i386 -display none -d cpu_reset,guest_errors -smp 1 -S -s  -m 512 -serial mon:stdio -kernel obj/crazierl.elf -append $(RTLD) -initrd obj/initrd

noisy: obj/crazierl.elf obj/initrd
	qemu-system-i386 -display none -smp 2 -d nochain,exec,cpu_reset,guest_errors -s  -m 256 -serial mon:stdio -kernel obj/crazierl.elf -append $(RTLD) -initrd obj/initrd

dist: .erlang.cookie obj/crazierl_epmd.beam $(OTPDIR)/bin/erl obj/gen_tcp_dist.beam
	$(OTPDIR)/bin/erl -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -sname host -pz $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)

dist-hw: .erlang.cookie obj/crazierl_epmd.beam $(OTPDIR)/bin/erl obj/gen_tcp_dist.beam
	$(OTPDIR)/bin/erl -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -name host -pz $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie) -kernel inet_dist_listen_min 4370

remote-shell: .erlang.cookie obj/crazierl_epmd.beam $(OTPDIR)/bin/erl obj/gen_tcp_dist.beam
	$(OTPDIR)/bin/erl -hidden -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -remsh 'crazierl@localhost' -sname shell -pz $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)

remote-shell-hw: .erlang.cookie obj/crazierl_epmd.beam $(OTPDIR)/bin/erl obj/gen_tcp_dist.beam
	$(OTPDIR)/bin/erl -hidden -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -remsh $(HWNODE) -name shell -pz $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)

push-code: $(TCPIP_OBJS) $(INITRD_ERLANG_OBJS) $(OTPDIR)/bin/escript
	@ERL_FLAGS="-hidden -sname pusher -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -pa $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)" $(OTPDIR)/bin/escript scripts/push_code.escript 'crazierl@localhost' $(TCPIP_OBJS) $(INITRD_ERLANG_OBJS)

timer-offset: $(OTPDIR)/bin/escript
	@ERL_FLAGS="-hidden -sname timer -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -pa $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)" $(OTPDIR)/bin/escript scripts/timer_offset.escript 'crazierl@localhost'

timer-offset-hw: $(OTPDIR)/bin/escript
	@ERL_FLAGS="-hidden -name timer -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -pa $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)" $(OTPDIR)/bin/escript scripts/timer_offset.escript $(HWNODE)

push-code-hw: $(TCPIP_OBJS) $(INITRD_ERLANG_OBJS) $(OTPDIR)/bin/escript
	@ERL_FLAGS="-hidden -name pusher -no_epmd -proto_dist gen_tcp -epmd_module crazierl_epmd -pa $(shell pwd)/$(OBJDIR) -setcookie $(shell cat .erlang.cookie)" $(OTPDIR)/bin/escript scripts/push_code.escript $(HWNODE) $(TCPIP_OBJS) $(INITRD_ERLANG_OBJS)

debugger:
	gdb -ex "set confirm off" -ex "add-symbol-file obj/crazierl.elf" -ex "add-symbol-file $$(find $(OTPDIR) -name beam.smp)" -ex "target remote localhost:1234"

.PHONY: clean $(OTPDIR)/bin/erlc
clean:
	rm -f obj/initrd obj/crazierl.elf obj/*.gz obj/*.o obj/*.beam obj/*.so obj/initrd.tmp obj/.deps/*.d obj/*.app obj/*.iso obj/iso/initrd obj/iso/crazierl.elf obj/iso/boot/grub/grub.cfg

erlang-runtime$(ERLANG_VERSION)/usr/share/keys/pkg/trusted/.setup:
	mkdir -p erlang-runtime$(ERLANG_VERSION)/usr/share/keys/pkg
	cp -a /usr/share/keys/pkg/trusted erlang-runtime$(ERLANG_VERSION)/usr/share/keys/pkg
	touch erlang-runtime$(ERLANG_VERSION)/usr/share/keys/pkg/trusted/.setup

$(OTPDIR)/bin/erl: erlang-runtime$(ERLANG_VERSION)/usr/share/keys/pkg/trusted/.setup
	IGNORE_OSVERSION=yes INSTALL_AS_USER=1 pkg -R ../cfg --root erlang-runtime$(ERLANG_VERSION) -o ABI=FreeBSD:14:i386 install -r latest -y erlang-runtime$(ERLANG_VERSION)
	touch $(OTPDIR)/bin/erl


$(OTPDIR)/bin/erl.patched: $(OTPDIR)/bin/erl
	sed -e 's@"/usr/local/lib/erlang$(ERLANG_VERSION)"@"$(shell pwd)/$(OTPDIR)"@' -i backup $(OTPDIR)/bin/erl
	touch $(OTPDIR)/bin/erl.patched

$(OTPDIR)/bin/erlc: $(OTPDIR)/bin/erl.patched
$(OTPDIR)/bin/escript: $(OTPDIR)/bin/erlc

ALL_KERNEL_OBJS = $(KERNEL_OBJS) $(FBSD_KERNEL_OBJS) $(BEARSSL_OBJS) obj/start.o
$(OBJDIR)/crazierl.elf: $(ALL_KERNEL_OBJS) kernel_src/linker.ld
	clang -m32 -g -static -ffreestanding -nostdlib -Xlinker -Tkernel_src/linker.ld -Xlinker $(ALL_KERNEL_OBJS)  -o obj/crazierl.elf -gdwarf-2

obj/start.o: kernel_src/start.s | $(DEPDIR)
	clang -m32 -g -gdwarf-2 -c $^ -o $@

INITRD_FILES := .erlang.cookie cfg/inetrc obj/etcpip.app /etc/termcap $(NIF_OBJS) obj/checksum.so $(TCPIP_OBJS) $(INITRD_ERLANG_OBJS)

.erlang.cookie: scripts/gen_cookie.escript $(OTPDIR)/bin/escript
	$(OTPDIR)/bin/escript scripts/gen_cookie.escript > .erlang.cookie.tmp
	mv .erlang.cookie.tmp .erlang.cookie

obj/initrd: scripts/hardcode_files.pl scripts/extract_start.escript $(OTPDIR)/bin/escript $(INITRD_FILES) Makefile
	./scripts/hardcode_files.pl beam.smp $(RTLD) $(OTPDIR) \
		OTPDIR/lib/crypto-*/ebin/crypto.beam OTPDIR/lib/crypto-*/priv/lib/crypto.so OTPDIR/lib/crypto-*/priv/lib/crypto_callback.so \
		OTPDIR/lib/runtime_tools-*/ebin/dbg.beam \
		$(INITRD_FILES) > obj/initrd.tmp
	mv obj/initrd.tmp obj/initrd


obj/etcpip.app: cfg/etcpip.app
	cp $< $@

erlang-tcpip/src:
	@echo "erlang-tcp submodule isn't checked out; you must run the following command:"
	@echo "git submodule init && git submodule update "
	@exit 1

$(TCPIP_SRCS): erlang-tcpip/src
$(TCPIP_OBJS): $(OBJDIR)/%.beam : erlang-tcpip/src/%.erl $(DEPDIR)/%.d | $(DEPDIR) $(OTPDIR)/bin/erlc
	$(OTPDIR)/bin/erlc -o $(OBJDIR)/ -MMD -MF $(DEPDIR)/$*.d $<

$(ERLANG_OBJS): $(OBJDIR)/%.beam : src/%.erl $(DEPDIR)/%.d | $(DEPDIR) $(OTPDIR)/bin/erlc
	$(OTPDIR)/bin/erlc -o $(OBJDIR)/ -MMD -MF $(DEPDIR)/$*.d $<

$(ERLANG_OVERRIDE_OBJS): $(OBJDIR)/%.beam : overrides/%.erl $(DEPDIR)/%.d $(OBJDIR)/hook_module.beam | $(DEPDIR) $(OTPDIR)/bin/erlc
	$(OTPDIR)/bin/erlc -I $(OTPDIR)/lib/kernel-*/include -pz $(shell pwd)/$(OBJDIR)/ -o $(OBJDIR)/ -MMD -MF $(DEPDIR)/$*.d '+{parse_transform,hook_module}' $<

$(KERNEL_OBJS): $(OBJDIR)/%.o: kernel_src/%.c $(DEPDIR)/%.c.d | $(DEPDIR)
	$(KERNEL_COMPILER) $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $< -I /usr/src/libexec/rtld-elf/ -I /usr/src/sys/ -I /usr/src/contrib/bearssl/inc/ -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

$(FBSD_KERNEL_OBJS): $(OBJDIR)/%.o: $(DEPDIR)/%.c.d | $(DEPDIR)
	$(KERNEL_COMPILER) -I kernel_src/ $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $(subst __,/,/usr/src/$*.c) -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

$(BEARSSL_OBJS): $(OBJDIR)/%.o: $(DEPDIR)/%.c.d | $(DEPDIR)
	$(KERNEL_COMPILER) -I /usr/src/contrib/bearssl/src/ -I /usr/src/contrib/bearssl/inc/ $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $(subst __,/,/usr/src/contrib/bearssl/src/$*.c) -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

$(NIF_OBJS): $(OBJDIR)/%.so: c_src/%.c $(OTPDIR)/bin/erl $(DEPDIR)/%.c.d | $(DEPDIR)
	$(NIF_COMPILER) $(DEPFLAGS) -MF $(DEPDIR)/$*.c.d.T $< -o $@
	mv -f $(DEPDIR)/$*.c.d.T $(DEPDIR)/$*.c.d && touch $@

erlang-tcpip/c_src/checksum.c: erlang-tcpip/src

obj/checksum.so: erlang-tcpip/c_src/checksum.c
	$(NIF_COMPILER) $< -o $@

$(DEPDIR): ; @mkdir -p $@
DEPFILES := $(ERLANG_OBJS:$(OBJDIR)/%.beam=$(DEPDIR)/%.d) $(ERLANG_OVERRIDE_OBJS:$(OBJDIR)/%.beam=$(DEPDIR)/%.d) $(TCPIP_OBJS:$(OBJDIR)/%.beam=$(DEPDIR)/%.d) $(KERNEL_SRCS:kernel_src/%.c=$(DEPDIR)/%.c.d) $(FBSD_KERNEL_SRCS:%.c=$(DEPDIR)/%.c.d) $(BEARSSL_SRCS:%.c=$(DEPDIR)/%.c.d) $(NIF_SRCS:c_src/%.c=$(DEPDIR)/%.c.d)
$(DEPFILES):
include $(wildcard $(DEPFILES))
