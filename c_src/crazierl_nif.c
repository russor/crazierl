#include <stdint.h>
#include <strings.h>
#include <sys/time.h>
#include <sys/timex.h>
#include <errno.h>

#include "erl_nif.h"
#include "kern_mmap.h"

ErlNifResourceType *MMAP_TYPE;
struct mmap_resource {
	uintptr_t start;
	size_t length;
};

ErlNifResourceType *IOPORT_TYPE;
struct ioport_resource {
	uint16_t start;
	uint16_t length;
};

static ERL_NIF_TERM inb_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int port;
	uint8_t ret;
	if (!enif_get_uint(env, argv[0], &port)) { return enif_make_badarg(env); }
	asm volatile ( "inb %1, %0" : "=a"(ret) : "Nd"((uint16_t)port) );
	return enif_make_uint(env, ret);
}

static ERL_NIF_TERM inl_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int port;
	uint32_t ret;
	if (!enif_get_uint(env, argv[0], &port)) { return enif_make_badarg(env); }
	asm volatile ( "inl %1, %0" : "=a"(ret) : "Nd"((uint16_t)port) );
	return enif_make_uint(env, ret);
}

static ERL_NIF_TERM outb_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int port, val;
	if (!enif_get_uint(env, argv[0], &port)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[1], &val)) { return enif_make_badarg(env); }
	asm volatile ( "outb %0, %1" : : "a"((uint8_t)val), "Nd"((uint16_t)port) );
	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM outl_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int port, val;
	if (!enif_get_uint(env, argv[0], &port)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[1], &val)) { return enif_make_badarg(env); }
	asm volatile ( "outl %0, %1" : : "a"((uint32_t)val), "Nd"((uint16_t)port) );
	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM map_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	uintptr_t start;
	size_t length;
	if (!enif_get_uint(env, argv[0], &start)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[1], &length)) { return enif_make_badarg(env); }

	void * ret;
	if (start) {
		ret = mmap((void *)start, length, PROT_READ | PROT_WRITE | PROT_FORCE, 0, -1, 0);
	} else {
		ret = mmap((void *)start, length, PROT_READ | PROT_WRITE, 0, -1, 0);
	}

	if ((start != 0 && ((uintptr_t) ret == start)) ||
	    (start == 0 && ((uintptr_t) ret != start))) {
		struct mmap_resource *resource = enif_alloc_resource(MMAP_TYPE, sizeof(struct mmap_resource));;
		resource->start = (uintptr_t) ret;
		resource->length = length;
		ERL_NIF_TERM ret = enif_make_tuple2(env,
			enif_make_atom(env, "ok"),
			enif_make_resource(env, resource)
		);
		enif_release_resource(resource);
		return ret;
	} else {
		return enif_make_atom(env, "error");
	}
}

static ERL_NIF_TERM map_port_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int start;
	unsigned int length;
	if (!enif_get_uint(env, argv[0], &start)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[1], &length)) { return enif_make_badarg(env); }

	if (start > UINT16_MAX || length > UINT16_MAX) {
		return enif_make_badarg(env);
	}

	struct ioport_resource *resource = enif_alloc_resource(IOPORT_TYPE, sizeof(struct ioport_resource));;
	resource->start = (uint16_t) start;
	resource->length = (uint16_t) length;
	ERL_NIF_TERM ret = enif_make_tuple2(env,
		enif_make_atom(env, "ok"),
		enif_make_resource(env, resource)
	);
	enif_release_resource(resource);
	return ret;
}

static ERL_NIF_TERM map_addr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	struct mmap_resource *resource;
	if (!enif_get_resource(env, argv[0], MMAP_TYPE, (void **)&resource)) { return enif_make_badarg(env); }
	uintptr_t physical = resource->start;
	return enif_make_tuple2(env,
	                        enif_make_uint64(env, resource->start),
	                        enif_make_uint64(env, physical));
}

static ERL_NIF_TERM bcopy_to_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	struct mmap_resource *mmap_resource;
	struct ioport_resource *ioport_resource;
	uintptr_t offset;
	ErlNifBinary binary;
	if (!enif_get_uint(env, argv[1], &offset)) { return enif_make_badarg(env); }
	if (!enif_inspect_iolist_as_binary(env, argv[2], &binary)) { return enif_make_badarg(env); }
	if (enif_get_resource(env, argv[0], MMAP_TYPE, (void **)&mmap_resource)) {
		if ((offset + binary.size) > mmap_resource->length) { return enif_make_badarg(env); }

		bcopy(binary.data, (void *)(mmap_resource->start + offset), binary.size);
		__sync_synchronize();

		return enif_make_atom(env, "ok");
	} else if (enif_get_resource(env, argv[0], IOPORT_TYPE, (void **)&ioport_resource)) {
		if ((offset + binary.size) > ioport_resource->length) { return enif_make_badarg(env); }

		uint16_t port = ioport_resource->start + offset;
		switch (binary.size) {
			case 1:
				asm volatile ( "outb %0, %1" : : "a"(binary.data[0]), "Nd"(port) );
				break;
			case 2: {
				uint16_t data = binary.data[1] << 8 | binary.data[0];
				asm volatile ( "outw %0, %1" : : "a"(data), "Nd"(port) );
				break;
			}
			case 4: {
				uint32_t data = binary.data[3] << 24 | binary.data[2] << 16 | binary.data[1] << 8 | binary.data[0];
				asm volatile ( "outl %0, %1" : : "a"(data), "Nd"(port) );
				break;
			}
			case 8: {
				uint32_t data = binary.data[3] << 24 | binary.data[2] << 16 | binary.data[1] << 8 | binary.data[0];
				asm volatile ( "outl %0, %1" : : "a"(data), "Nd"(port) );
				port += 4;
				data = binary.data[7] << 24 | binary.data[6] << 16 | binary.data[5] << 8 | binary.data[4];
				asm volatile ( "outl %0, %1" : : "a"(data), "Nd"(port) );
				break;
			}
			default:
				printf("bad binary size for bcopy_to %d\r\n", binary.size);
				return enif_make_badarg(env);
				/*
				for (size_t i = 0; i < binary.size; ++i) {
					uint16_t port = ioport_resource->start + offset + i;
					asm volatile ( "outb %0, %1" : : "a"(binary.data[i]), "Nd"(port) );
				}*/
		}
		return enif_make_atom(env, "ok");
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM bcopy_from_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	struct mmap_resource *mmap_resource;
	struct ioport_resource *ioport_resource;
	uintptr_t offset;
	size_t length;
	if (!enif_get_uint(env, argv[1], &offset)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[2], &length)) { return enif_make_badarg(env); }
	if (enif_get_resource(env, argv[0], MMAP_TYPE, (void **)&mmap_resource)) {
		if ((offset + length) > mmap_resource->length) { return enif_make_badarg(env); }

		ERL_NIF_TERM binary;
		unsigned char * bindata = enif_make_new_binary(env, length, &binary);

		__sync_synchronize();
		bcopy((void *) (mmap_resource->start + offset), bindata, length);
		return binary;
	} else if (enif_get_resource(env, argv[0], IOPORT_TYPE, (void **)&ioport_resource)) {
		if ((offset + length) > ioport_resource->length) { return enif_make_badarg(env); }

		ERL_NIF_TERM binary;
		uint16_t port = ioport_resource->start + offset;
		unsigned char * bindata = enif_make_new_binary(env, length, &binary);
		switch (length) {
			case 1:
				asm volatile ( "inb %1, %0" : "=a"(bindata[0]) : "Nd"(port) );
				break;
			case 2: {
				uint16_t data;
				asm volatile ( "inw %1, %0" : "=a"(data) : "Nd"(port) );
				bindata[0] = data & 0xFF;
				bindata[1] = (data >> 8) & 0xFF;
				break;
			}
			case 4: {
				uint32_t data;
				asm volatile ( "inl %1, %0" : "=a"(data) : "Nd"(port) );
				bindata[0] = data & 0xFF;
				bindata[1] = (data >> 8) & 0xFF;
				bindata[2] = (data >> 16) & 0xFF;
				bindata[3] = (data >> 24) & 0xFF;
				break;
			}
			default:
				for (size_t i = 0; i < length; ++i, ++port) {
					asm volatile ( "inb %1, %0" : "=a"(bindata[i]) : "Nd"(port) );
				}
		}
		return binary;
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM time_offset_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	struct timeval tv, tv2;

#if defined(__i386__)
	_Static_assert(sizeof(tv.tv_sec) == sizeof(int), "time_t is not an int on i386?");
	if (!enif_get_int(env, argv[0], &tv.tv_sec)) { return enif_make_badarg(env); }
#else
	_Static_assert(sizeof(tv.tv_sec) == sizeof(int64_t), "time_t is not a int64 on non-i386?");
	if (!enif_get_int64(env, argv[0], &tv.tv_sec)) { return enif_make_badarg(env); }
#endif

	_Static_assert(sizeof(tv.tv_usec) == sizeof(long), "suseconds_t is not a long?");
	if (!enif_get_long(env, argv[1], &tv.tv_usec)) { return enif_make_badarg(env); }

	if (gettimeofday(&tv2, NULL) != 0) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, errno));
	}
	tv2.tv_sec += tv.tv_sec;
	tv2.tv_usec += tv.tv_usec;
	while (tv2.tv_usec >= 1000000) {
		++tv2.tv_sec;
		tv2.tv_usec -= 1000000;
	}
	while (tv2.tv_usec < 0) {
		--tv2.tv_sec;
		tv2.tv_usec += 1000000;
	}
	if (settimeofday(&tv2, NULL) != 0) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, errno));
	}
	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM ntp_adjtime_freq_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	struct timex tx = {0};
	int result;
	if (!enif_get_long(env, argv[0], &tx.freq)) { return enif_make_badarg(env); }
	tx.modes = MOD_FREQUENCY;
	result = ntp_adjtime(&tx);
	if (result != TIME_OK) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, errno));
	}
	return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"inb", 1, inb_nif},
    {"inl", 1, inl_nif},
    {"outb", 2, outb_nif},
    {"outl", 2, outl_nif},
    {"map", 2, map_nif},
    {"map_port", 2, map_port_nif},
    {"map_addr", 1, map_addr_nif},
    {"bcopy_to", 3, bcopy_to_nif},
    {"bcopy_from", 3, bcopy_from_nif},
    {"time_offset", 2, time_offset_nif},
    {"ntp_adjtime_freq", 1, ntp_adjtime_freq_nif}
};

int load (ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	MMAP_TYPE = enif_open_resource_type(env, NULL, "mmap", NULL, ERL_NIF_RT_CREATE, NULL);
	IOPORT_TYPE = enif_open_resource_type(env, NULL, "ioport", NULL, ERL_NIF_RT_CREATE, NULL);
	return 0;
}

ERL_NIF_INIT(crazierl, nif_funcs, load, NULL, NULL, NULL)
