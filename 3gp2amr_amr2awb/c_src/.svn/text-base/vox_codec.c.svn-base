#include <stdint.h>
#include <erl_nif.h>

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))
#define range_limit(x, lower, upper) (min(max(x, lower), upper))

#define min_sample -0x8000
#define max_sample 0x7fff

#define MAX_BIN_SIZE 4800000

typedef struct {
  int last_output;
  int step_index;
  int errors;
} adpcm_t;

static int const oki_steps[49] = { /* ~12-bit precision; 4 bit code */
  256, 272, 304, 336, 368, 400, 448, 496, 544, 592, 656, 720, 800, 880, 960,
  1056, 1168, 1280, 1408, 1552, 1712, 1888, 2080, 2288, 2512, 2768, 3040, 3344,
  3680, 4048, 4464, 4912, 5392, 5936, 6528, 7184, 7904, 8704, 9568, 10528,
  11584, 12736, 14016, 15408, 16960, 18656, 20512, 22576, 24832};

static int const step_changes[8] = {-1, -1, -1, -1, 2, 4, 6, 8};

static const int max_step_index = 48;
static const int sign = 8;
//static const unsigned sign_mask = 0x08;
static const unsigned code_mask = 0x07;
static const int decode_shift = 3;
static const int mask = ~15;

int16_t vox_decode(uint8_t code, adpcm_t* status) {
    int s = ((code & code_mask) << 1) | 1;
    s = ((oki_steps[status->step_index] * s) >> decode_shift) & mask;
    if (code & sign) {
        s = -s;
    }
    int values_with_last = s + status->last_output;
    if (values_with_last < min_sample || values_with_last > max_sample) {
        int grace = (oki_steps[status->step_index] >> decode_shift) & mask;
        if (values_with_last < min_sample - grace || values_with_last > max_sample + grace) {
          status->errors++;
        }
        values_with_last = values_with_last < min_sample? min_sample : max_sample;
    }
    status->step_index += step_changes[code & code_mask];
    status->step_index = range_limit(status->step_index, 0, max_step_index);
    status->last_output = values_with_last;
    return values_with_last;
}

static ERL_NIF_TERM vox8k2pcm8k16(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary orig_bin;
    if (!enif_inspect_binary(env, argv[0], &orig_bin)) {
	return enif_make_badarg(env);
    }
    if (orig_bin.size > MAX_BIN_SIZE) {
	return enif_make_badarg(env);
    }

    ERL_NIF_TERM ret;
    size_t length = orig_bin.size;
    int16_t* dest = (int16_t*)enif_make_new_binary(env, length*4, &ret);
    uint8_t *adpcm = (uint8_t*)(orig_bin.data);
    adpcm_t status = {0,0,0};
    size_t i = 0;
    for (i = 0; i < length; ++i) {
        dest[i*2] = vox_decode(adpcm[i]>>4, &status);
        dest[i*2+1] = vox_decode(adpcm[i], &status);
    }
    return ret;
}

static ErlNifFunc nif_funcs[] =
{
    {"vox8k2pcm8k16", 1, vox8k2pcm8k16}
};

ERL_NIF_INIT(vox_codec,nif_funcs,NULL,NULL,NULL,NULL)
