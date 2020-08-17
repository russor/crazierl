-record (pci_common, {
	bus, device, function, driver, pid,
	vendor, device_id, class, sub_class,
	revision, prog_if, capabilities
}).

-record (pci_device, {
	common, chip_vendor, chip_device_id,
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
