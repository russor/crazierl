-record (pci_common, {
	bus, device, function,
	vendor, device_id, class, sub_class,
	revision, prog_if, capabilities
}).

-record (pci_device, {
	common, subsystem_vendor, subsystem_device,
	interrupt_line, interrupt_pin,
	bar0, bar1, bar2, bar3, bar4, bar5
}).

-record (pci_bridge, {
	common, bar0, bar1,
	secondary_bus
}).

-record (pci_mem_bar, {
	base, size, prefetch, type
}).

-record (pci_io_bar, {
	base, size
}).
