
#include "common.h"
#include "bearssl_hash.h"
#include "bearssl_rand.h"
#include "acpi.h"

DECLARE_LOCK(rand);
br_hmac_drbg_context drbg;
br_hmac_drbg_context drbgs[MAX_CPUS];
__thread int seedings = 256;

void rand_init() {
    LOCK(&rand);
    br_hmac_drbg_init(&drbg, &br_sha256_vtable, NULL, 0);
    for (int i = 0; i < numcpu; ++i) {
        br_hmac_drbg_init(&drbgs[i], &br_sha256_vtable, NULL, 0);
    }
    UNLOCK(&rand);
}

void rand_bytes(void * out, size_t len) {
    if (seedings) {
        char buf[br_sha256_SIZE];
        LOCK(&rand);
        br_hmac_drbg_generate(&drbg, buf, sizeof(buf));
        UNLOCK(&rand);
        br_hmac_drbg_update(&drbgs[current_cpu], buf, sizeof(buf));
    }
    br_hmac_drbg_generate(&drbg, out, len);
}

void rand_update(const void * seed, size_t len) {
    br_hmac_drbg_update(&drbgs[current_cpu], seed, len);
    if (seedings) {
        --seedings;
        char buf[br_sha256_SIZE];
        br_hmac_drbg_generate(&drbgs[current_cpu], buf, sizeof(buf));
        LOCK(&rand);
        br_hmac_drbg_update(&drbg, buf, sizeof(buf));
        br_hmac_drbg_generate(&drbg, buf, sizeof(buf));
        UNLOCK(&rand);
        br_hmac_drbg_update(&drbgs[current_cpu], buf, sizeof(buf));
    }
}
