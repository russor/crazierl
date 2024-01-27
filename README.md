# [Crazierl](https://crazierl.org)

**Crazierl** is an Erlang operating system.

**Erlang** is a programming language and runtime system for building massively scalable soft real-time systems with requirements on high availability.

[Learn more about Erlang and OTP](http://erlang.org/doc/system_architecture_intro/sys_arch_intro.html).

## What does it do

[See the demo](https://crazierl.org/demo.html)

Crazierl is just enough of an OS to run BEAM, the Erlang/OTP VM for x86 PCs
(and virtual machines). The console is the Erlang shell. Most drivers will
be written in Erlang, and can be hot loaded.

Drivers are present for text mode vga, pc keyboard, pc com port, virtio-net,
and rtl_8168.

erlang-tcpip enables a basic tcp-ip stack, and there is an example dhcp
client, ntp client, and simple http server.

Dist works (milestone 2), with -proto_dist gen_tcp, a custom epmd, and a little help here
and there with cookies and things.

Note: crazierl does not support ne2000 nics, so there is currently no networking for crazierl in
the demo using v86.

## Where does it run

Runs on qemu, v86, and some real hardware. Needs a multiboot loader (tested
on the built in multiboot loader for qemu and v86, as well as ipxe and
grub). ACPI required, CPU support for SSE is required. The TSC is treated as
invariant, but it's not a fatal error, as many VMs don't support it. SMP
works (milestone 1), but is untested at large core counts; over 256 cores is definitely
not going to work.

## How does it work

Crazierl provides a (roughly) FreeBSD compatible syscall interface, and can
load and run FreeBSD executables from an initrd filesystem.  The Makefile
will prepare an initrd that includes many files from the pkg distribution of
Erlang, the FreeBSD rtld, and object files from this source, including some
which override files in the Erlang distribution.

## How to build

Should build with gmake on FreeBSD matching the version returned from
syscall\_\_\_sysctl in kernel_src/kernel.c This will use Erlang/OTP from pkg,
but you can change OPTDIR in Makefile to use a different build. Using a
checked build can be very helpful for finding kernel bugs that manifest in
returning improper results to BEAM. Crazierl is (currently) 32-bit, but can
build on 32-bit or 64-bit FreeBSD. Send me email if you have trouble
building.

## What is it based on

Erlang/OTP provides for the userland and was the inspiration for the
project.

FreeBSD provides many things: the syscall interface, libraries and
utilities, rtld: the dynamic loader.

BearSSL, from FreeBSD contrib, provides the blocks to build kernel random.

Erlang-Tcpip provides the tcp-ip stack.

GNU Make builds it all.

Initial structure of the kernel is from
https://wiki.osdev.org/User:Zesterer/Bare_Bones as well as lots of other
knowledge from the OS Dev wikis and forums; thank you!

## Similar projects

[Ling](https://github.com/cloudozer/ling): ErlangOnXen -- an alternate VM
for Erlang targetted to be a Xen guest.

[HydrOS](http://www.erlang-factory.com/euc2017/sam-williams) presentation
available; but I didn't find code? Seems to have a focus on a capability
security model. Presentation includes some additional projects.

## Why

An excellent question. It seemed like a good idea to explore the possibility
of Erlang as its own system.

## Who did this

[Richard Russo](mailto:crazierl@ruka.org) and friends.

## License

Copyright \[2019-2024\]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
