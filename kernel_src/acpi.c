#include <contrib/dev/acpica/include/platform/acenv.h>
#include <contrib/dev/acpica/include/platform/acfreebsd.h>
#include <contrib/dev/acpica/include/actypes.h>
#include <contrib/dev/acpica/include/actbl.h>
#include "common.h"
#include "acpi.h"
#include "apic.h"

unsigned int numcpu;


struct cpu cpus[MAX_CPUS];

uint8_t acpi_checksum(uint8_t * p, ssize_t len) {
	uint8_t ret = 0;
	while (len) {
		ret += *p;
		++p;
		--len;
	}
	return ret;
}

int acpi_check_table(void * c) {
	ACPI_TABLE_HEADER *t = (ACPI_TABLE_HEADER *)c;
	return acpi_checksum(c, t->Length) == 0;
}

void * acpi_find_table(void * name, void * rsdt) {
	ACPI_TABLE_HEADER *t = (ACPI_TABLE_HEADER *)rsdt;
	if (bcmp(t->Signature, ACPI_SIG_XSDT, ACPI_NAMESEG_SIZE) == 0) {
		ACPI_TABLE_XSDT * table = (ACPI_TABLE_XSDT *)rsdt;
		for (int i = 0; (i * sizeof(table->TableOffsetEntry[0])) + sizeof(table->Header) < table->Header.Length; ++i) {
			if (bcmp(((ACPI_TABLE_HEADER*)table->TableOffsetEntry[i])->Signature, name, ACPI_NAMESEG_SIZE) == 0) {
				return  (void *)table->TableOffsetEntry[i];
			}
		}
		DEBUG_PRINTF("couldn't find %s in XSDT\r\n", name);
		return NULL;
	} else if (bcmp(t->Signature, ACPI_SIG_RSDT, ACPI_NAMESEG_SIZE) == 0) {
		ACPI_TABLE_RSDT * table = (ACPI_TABLE_RSDT *)rsdt;
		for (int i = 0; (i * sizeof(table->TableOffsetEntry[0])) + sizeof(table->Header) < table->Header.Length; ++i) {
			if (bcmp(((ACPI_TABLE_HEADER*)table->TableOffsetEntry[i])->Signature, name, ACPI_NAMESEG_SIZE) == 0) {
				return  (void *)table->TableOffsetEntry[i];
			}
		}
		DEBUG_PRINTF("couldn't find %s in RSDT\r\n", name);
		return NULL;
	} else {
		EARLY_ERROR_PRINTF("RSDT is not an RSDT or XSDT\r\n");
		return NULL;
	}
}

// this is part ACPI parsing, but mostly kernel setup
int acpi_process_madt(void * rsdt) {
	numcpu = 0; timer_gsirq = 0; io_apic_count = 0;
	ACPI_TABLE_MADT *madt = (ACPI_TABLE_MADT *)acpi_find_table(ACPI_SIG_MADT, rsdt);
	if (madt == NULL) {
		EARLY_ERROR_PRINTF("couldn't find MADT\r\n");
		return 0;
	}
	if (!(acpi_checksum((void *) madt, madt->Header.Length) == 0)) {
		EARLY_ERROR_PRINTF("MADT checksum failed\r\n");
		return 0;
	}
	local_apic = madt->Address;
	void *p = ((void *)madt) + sizeof(*madt);
	while (p < (((void *)madt) + madt->Header.Length)) {
		ACPI_SUBTABLE_HEADER *subhead = (ACPI_SUBTABLE_HEADER *)p;
		if (subhead->Type == ACPI_MADT_TYPE_LOCAL_APIC && subhead->Length == sizeof(ACPI_MADT_LOCAL_APIC)) {
			ACPI_MADT_LOCAL_APIC *data = (ACPI_MADT_LOCAL_APIC *)p;
			if (data->LapicFlags & ACPI_MADT_ENABLED) {
				if (numcpu < MAX_CPUS) {
					EARLY_ERROR_PRINTF("Processor %d, APIC %d, Flags %x\r\n", data->ProcessorId, data->Id, data->LapicFlags);
					cpus[numcpu].flags = CPU_ENABLED;
					cpus[numcpu].apic_id = data->Id;
					++numcpu;
				} else {
					EARLY_ERROR_PRINTF("Ignoring processor %d, APIC %d; more than MAX_CPUS (%d)\r\n", data->ProcessorId, data->Id, MAX_CPUS);
				}
			} else {
				EARLY_ERROR_PRINTF("DISABLED! Processor %d, APIC %d, Flags %x\r\n", data->ProcessorId, data->Id, data->LapicFlags);
			}
		} else if (subhead->Type == ACPI_MADT_TYPE_IO_APIC && subhead->Length == sizeof(ACPI_MADT_IO_APIC)) {
			ACPI_MADT_IO_APIC *data = (ACPI_MADT_IO_APIC *)p;
			if (io_apic_count == MAX_IO_APICS) {
				EARLY_ERROR_PRINTF("Too many IO-APICS (limit %d); recompile with a higher limit for your machine!\r\n", MAX_IO_APICS);
				return 0;
			}
			io_apics[io_apic_count].address = (volatile uint32_t *)data->Address;
			io_apics[io_apic_count].base = data->GlobalIrqBase;

			io_apics[io_apic_count].address[0] = 0;
			uint32_t d = io_apics[io_apic_count].address[4];
			d >>= 24;
			d &= 0x0F;
			if (d != data->Id) {
				EARLY_ERROR_PRINTF("IO-APIC id (%d) doesn't match id from MADT (%d)\r\n", d, data->Id);
				//return 0;
			}
			io_apics[io_apic_count].address[0] = 1;
			d = io_apics[io_apic_count].address[4];
			d >>= 16;
			d &= 0xFF;

			io_apics[io_apic_count].numintr = d + 1;
			++io_apic_count;
		} else if (subhead->Type == ACPI_MADT_TYPE_INTERRUPT_OVERRIDE && subhead->Length == sizeof(ACPI_MADT_INTERRUPT_OVERRIDE)) {
			ACPI_MADT_INTERRUPT_OVERRIDE *data = (ACPI_MADT_INTERRUPT_OVERRIDE *)p;
			if (data->Bus == 0 && data->SourceIrq == 0) {
				timer_gsirq = data->GlobalIrq;
				timer_flags = data->IntiFlags;
				EARLY_ERROR_PRINTF("timer irq is global IRQ %d (flags %x)\r\n", timer_gsirq, data->IntiFlags);
			} else {
				EARLY_ERROR_PRINTF("ISO: Bus %d, SourceIRQ %d, GlobalIRQ %d, Flags %x\r\n",
				             data->Bus, data->SourceIrq, data->GlobalIrq, data->IntiFlags);
			}

		} else if (subhead->Type == ACPI_MADT_TYPE_LOCAL_APIC_NMI && subhead->Length == sizeof(ACPI_MADT_LOCAL_APIC_NMI)) {
			ACPI_MADT_LOCAL_APIC_NMI *data = (ACPI_MADT_LOCAL_APIC_NMI *)p;
			EARLY_ERROR_PRINTF("NMI: Processor %d, Flags %x, Lint %d\r\n",
				data->ProcessorId, data->IntiFlags, data->Lint);
		} else {
			EARLY_ERROR_PRINTF("uknown MADT item, type %d, length %d\r\n", subhead->Type, subhead->Length);
		}
		p += subhead->Length;
	}
	return 1;
}

void * check_rsdp(void *p) {
	ACPI_RSDP_COMMON *acpiv1 = (ACPI_RSDP_COMMON *)p;
	ACPI_TABLE_RSDP *acpiv2 = (ACPI_TABLE_RSDP *)p;
	if (bcmp(ACPI_SIG_RSDP, p, sizeof(acpiv1->Signature)) != 0) { return NULL; }
	if (acpiv1->Revision == 0 && acpi_checksum(p, sizeof(*acpiv1)) == 0) {
		return (void *)acpiv1->RsdtPhysicalAddress;
	} else if (acpiv2->Revision == 2 && acpi_checksum(p, sizeof(*acpiv2)) == 0){
		return (void *)acpiv2->XsdtPhysicalAddress;
	} else {
		return NULL;
	}
}		

void * acpi_find_rsdt (void *hint) {
	void * ret;
	if (hint != NULL) {
		ret = check_rsdp(hint);
		if (ret) { return ret; }
	}
	void * search_start = (void *) 0xE0000;
	void * search_end   = (void *) 0x100000;
	
	void * ebda_segment = (void *) ( *(uint16_t *)0x40E << 4);
	if (ebda_segment <= search_start && ebda_segment + 1024 >= search_start) {
		search_start = ebda_segment;
	} else {
		for (hint = ebda_segment; hint < ebda_segment + 1024; hint+= 16) {
			ret = check_rsdp(hint);
			if (ret) { return ret; }
		}
	}
	for (hint = search_start; hint < search_end; hint+= 16) {
		ret = check_rsdp(hint);
		if (ret) { return ret;}
	}
	return NULL;
}
