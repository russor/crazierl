-module (virtio_net).

-include ("pci.hrl").
-export([check/2, attach/2]).

check(#pci_device{common = #pci_common{vendor = 16#1AF4, device_id = 16#1000}}, _Args) -> true;
check(#pci_device{common = #pci_common{vendor = 16#1AF4, device_id = 16#1041}}, _Args) -> true.

attach(Device, _Args) ->
	loop(Device).

loop(Device) ->
	timer:sleep(10000),
	loop(Device).
	