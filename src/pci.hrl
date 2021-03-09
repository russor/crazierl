-record (pci_common, {
	bus, device, function, driver, pid,
	vendor, device_id, class, sub_class,
	revision, prog_if, capabilities, msix_map
}).

-record (pci_device, {
	common, chip_vendor, chip_device_id,
	interrupt_line, interrupt_pin,
	bars
}).

-record (pci_bridge, {
	common, bars, secondary_bus
}).

-record (pci_mem_bar, {
	base, size, prefetch, type
}).

-record (pci_io_bar, {
	base, size
}).

-record (pci_msi_x, {
	enabled, mask, size, table, pending, offset
}).
