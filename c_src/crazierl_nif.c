#include <strings.h>
#include "erl_nif.h"
#include "kern_mmap.h"

static ERL_NIF_TERM inb_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int port;
	uint8_t ret;
	if (!enif_get_uint(env, argv[0], &port)) { return enif_make_badarg(env); }
	asm volatile ( "inb %1, %0" : "=a"(ret) : "Nd"((uint16_t)port) );
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

static ERL_NIF_TERM map_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	uintptr_t start;
	size_t length;
	if (!enif_get_uint(env, argv[0], &start)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[1], &length)) { return enif_make_badarg(env); }

	void * ret = mmap((void *)start, length, PROT_READ | PROT_WRITE | PROT_FORCE, 0, -1, 0);
	if ((uintptr_t) ret  == start) {
		return enif_make_atom(env, "ok");
	} else {
		return enif_make_atom(env, "error");
	}
}

static ERL_NIF_TERM bcopy_to_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	uintptr_t start;
	ErlNifBinary binary;
	if (!enif_get_uint(env, argv[0], &start)) { return enif_make_badarg(env); }
	if (!enif_inspect_binary(env, argv[1], &binary)) { return enif_make_badarg(env); }

	bcopy(binary.data, (void *)start, binary.size);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM bcopy_from_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	uintptr_t start;
	size_t length;
	if (!enif_get_uint(env, argv[0], &start)) { return enif_make_badarg(env); }
	if (!enif_get_uint(env, argv[1], &length)) { return enif_make_badarg(env); }

	ERL_NIF_TERM binary;
	unsigned char * bindata = enif_make_new_binary(env, length, &binary);

	bcopy((void *) start, bindata, length);

	return binary;
}

static ErlNifFunc nif_funcs[] = {
    {"inb", 1, inb_nif},
    {"outb", 2, outb_nif},
    {"map", 2, map_nif},
    {"bcopy_to", 2, bcopy_to_nif},
    {"bcopy_from", 2, bcopy_from_nif}
};

ERL_NIF_INIT(crazierl, nif_funcs, NULL, NULL, NULL, NULL)
