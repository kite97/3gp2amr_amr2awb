#include <erl_nif.h>
#include <stdio.h>
#include <stdint.h>
#define DATA_BUFFER_LEN_8K 160

static ERL_NIF_TERM mix_8khz(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM conferee_in_tuple = argv[0];
    int conferee_number = 0;
    ERL_NIF_TERM *conferee_in_data_terms;
    if(!enif_get_tuple(env, conferee_in_tuple, &conferee_number, (const ERL_NIF_TERM**)&conferee_in_data_terms)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary* conferee_in_data = enif_alloc(conferee_number * sizeof(ErlNifBinary));
    ErlNifBinary* conferee_out_data = enif_alloc(conferee_number * sizeof(ErlNifBinary));
    int16_t** in_data_streams = enif_alloc(conferee_number * sizeof(int16_t*));
    int16_t** out_data_streams = enif_alloc(conferee_number * sizeof(int16_t*));
    int32_t all_mixed;
    int32_t mixed_data;
    int i;
    int j;
    ERL_NIF_TERM* conferee_out_data_terms = enif_alloc(conferee_number * sizeof(ERL_NIF_TERM));
    for(i = 0; i < conferee_number; i++) {
        if(!enif_inspect_binary(env, conferee_in_data_terms[i], &(conferee_in_data[i]))) {
            return enif_make_badarg(env);
        }
        enif_alloc_binary((size_t)(conferee_in_data[i].size), &conferee_out_data[i]);
        in_data_streams[i] = (int16_t*)conferee_in_data[i].data;
        out_data_streams[i] = (int16_t*)conferee_out_data[i].data;
    }
    for(i = 0; i < DATA_BUFFER_LEN_8K; i++) {
        all_mixed = 0;
        for(j = 0; j < conferee_number; j++) {
            all_mixed += (int32_t)(in_data_streams[j][i]);
        }
        for(j = 0; j < conferee_number; j++) {
            mixed_data = all_mixed - (int32_t)in_data_streams[j][i];
            if(mixed_data > (int32_t)32767) {
                mixed_data = (int32_t)32767;
            } else if(mixed_data < (int32_t)-32768) {
                mixed_data = (int32_t)-32768;
            }
            out_data_streams[j][i] = (int16_t)mixed_data;
        }
    }
    for(i = 0; i < conferee_number; i++) {
        conferee_out_data_terms[i] = enif_make_binary(env, &conferee_out_data[i]);
    }
    ERL_NIF_TERM result = enif_make_tuple_from_array(env, conferee_out_data_terms, (unsigned int)conferee_number);
    enif_free(conferee_in_data);
    enif_free(conferee_out_data);
    enif_free(in_data_streams);
    enif_free(out_data_streams);
    enif_free(conferee_out_data_terms);
    return result;
}

static ERL_NIF_TERM mix_16khz(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_atom(env, "enospprt");
}

static ErlNifFunc nif_funcs[] = {
    {"mix_8khz", 1, mix_8khz},
    {"mix_16khz", 1, mix_16khz}
};

ERL_NIF_INIT(audio_mix,nif_funcs,NULL,NULL,NULL,NULL)
