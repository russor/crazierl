#include <contrib/dev/acpica/include/platform/acenv.h>
#include <contrib/dev/acpica/include/platform/acfreebsd.h>
#include <contrib/dev/acpica/include/actypes.h>
#include <contrib/dev/acpica/include/actbl.h>

uint8_t acpi_checksum(char * p, ssize_t len) {
	uint8_t ret = 0;
	while (len) {
		ret += *p;
		++p;
		--len;
	}
	return ret;
}

char * check_rsdp(char *p) {
	if (memcmp(ACPI_SIG_RSDP, p, sizeof(ACPI_SIG_RSDP)) != 0) { return NULL; }
	ACPI_RSDP_COMMON *acpiv1 = (ACPI_RSDP_COMMON *)p;
	ACPI_TABLE_RSDP *acpiv2 = (ACPI_TABLE_RSDP *)p;
	if (acpiv1->Revision == 0 && acpi_checksum(p, sizeof(*acpiv1))) {
		return (char *)acpiv1->RsdtPhysicalAddress;
	} else if (acpiv2->Revision == 2 && acpi_checksum(p, sizeof(*acpiv2))){
		return (char *)acpiv2->XsdtPhysicalAddress;
	} else {
		return NULL;
	}
}		

char * acpi_find_rsdt (char *hint) {
	char * ret;
	if (hint != NULL) {
		ret = check_rsdp(hint);
		if (ret) { return ret; }
	}
	char * search_start = (char *) 0xE0000;
	char * search_end   = (char *) 0xFFFFF;
	
	char * ebda_segment = (char *) ( *(uint16_t *)0x40E << 4);
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
