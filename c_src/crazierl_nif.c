#include "erl_nif.h"

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

static ErlNifFunc nif_funcs[] = {
    {"inb", 1, inb_nif},
    {"outb", 2, outb_nif}
};

ERL_NIF_INIT(crazierl, nif_funcs, NULL, NULL, NULL, NULL)
