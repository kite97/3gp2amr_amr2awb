#include <stdint.h>
#include <string.h>
#include <erl_nif.h>
#include <interf_enc.h>
#include <interf_dec.h>
#include <enc_if.h>
#include <dec_if.h>

#define SAMPLES_NB 160
#define SAMPLES_WB 320
#define MAX_LEN_NB 32
#define MAX_LEN_WB 61
#define ENC_STATE_LEN_NB sizeof(enc_interface_State)
#define DEC_STATE_LEN_NB sizeof(dec_interface_State)
#define ENC_STATE_LEN_WB sizeof(WB_enc_if_state)
#define DEC_STATE_LEN_WB sizeof(WB_dec_if_state)

static const int nb_block_size[]= {13, 14, 16, 18, 20, 21, 27, 32,  6, 0, 0, 0, 0, 0, 0, 1};
static const int wb_block_size[]= {18, 24, 33, 37, 41, 47, 51, 59, 61, 6, 6, 0, 0, 0, 1, 1};

static ERL_NIF_TERM amrnb_encode_frame(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary State;
    if (!enif_inspect_binary(env, argv[0], &State)) {
        return enif_make_badarg(env);
    }
    if (State.size != ENC_STATE_LEN_NB) {
        return enif_make_badarg(env);
    }
    ErlNifBinary Pcm;
    if (!enif_inspect_binary(env, argv[1], &Pcm)) {
        return enif_make_badarg(env);
    }
    if (Pcm.size != SAMPLES_NB*sizeof(int16_t)) {
        return enif_make_badarg(env);
    }
    int mode;
    if (!enif_get_int(env, argv[2], &mode)) {
        return enif_make_badarg(env);
    }
    if (mode < 0 || mode > 7) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM NState;
    enc_interface_State* new_state = (enc_interface_State*)enif_make_new_binary(env, ENC_STATE_LEN_NB, &NState);
    memcpy(new_state, State.data, ENC_STATE_LEN_NB);

    ERL_NIF_TERM Frame;
    unsigned char* frame = (unsigned char*)enif_make_new_binary(env, nb_block_size[mode], &Frame);

    Encoder_Interface_Encode(new_state, mode, (int16_t*)Pcm.data, frame, 0);

    return enif_make_tuple2(env, NState, Frame);
}
static ERL_NIF_TERM amrnb_decode_frame(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary State;
    if (!enif_inspect_binary(env, argv[0], &State)) {
        return enif_make_badarg(env);
    }
    if (State.size != DEC_STATE_LEN_NB) {
        return enif_make_badarg(env);
    }
    ErlNifBinary Frame;
    if (!enif_inspect_binary(env, argv[1], &Frame)) {
        return enif_make_badarg(env);
    }
    if (Frame.size > MAX_LEN_NB) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM NState;
    dec_interface_State* new_state = (dec_interface_State*)enif_make_new_binary(env, DEC_STATE_LEN_NB, &NState);
    memcpy(new_state, State.data, DEC_STATE_LEN_NB);

    ERL_NIF_TERM Pcm;
    int16_t* pcm = (int16_t*)enif_make_new_binary(env, SAMPLES_NB*sizeof(int16_t), &Pcm);

    Decoder_Interface_Decode(new_state, Frame.data, pcm, 0);

    return enif_make_tuple2(env, NState, Pcm);
}

static ERL_NIF_TERM amrnb_encode_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    enc_interface_State* state = (enc_interface_State*)enif_make_new_binary(env, ENC_STATE_LEN_NB, &ret);
    Encoder_Interface_init(state, 0);
    return ret;
}
static ERL_NIF_TERM amrnb_decode_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    dec_interface_State* state = (dec_interface_State*)enif_make_new_binary(env, DEC_STATE_LEN_NB, &ret);
    Decoder_Interface_init(state);
    return ret;
}

static ERL_NIF_TERM amrwb_encode_frame(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary State;
    if (!enif_inspect_binary(env, argv[0], &State)) {
        return enif_make_badarg(env);
    }
    if (State.size != ENC_STATE_LEN_WB) {
        return enif_make_badarg(env);
    }
    ErlNifBinary Pcm;
    if (!enif_inspect_binary(env, argv[1], &Pcm)) {
        return enif_make_badarg(env);
    }
    if (Pcm.size != SAMPLES_WB*sizeof(int16_t)) {
        return enif_make_badarg(env);
    }
    int mode;
    if (!enif_get_int(env, argv[2], &mode)) {
        return enif_make_badarg(env);
    }
    if (mode < 0 || mode > 8) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM NState;
    WB_enc_if_state* new_state = (WB_enc_if_state*)enif_make_new_binary(env, ENC_STATE_LEN_WB, &NState);
    memcpy(new_state, State.data, ENC_STATE_LEN_WB);

    ERL_NIF_TERM Frame;
    unsigned char* frame = (unsigned char*)enif_make_new_binary(env, wb_block_size[mode], &Frame);

    E_IF_encode(new_state, mode, (int16_t*)Pcm.data, frame, 0);

    return enif_make_tuple2(env, NState, Frame);
}
static ERL_NIF_TERM amrwb_decode_frame(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary State;
    if (!enif_inspect_binary(env, argv[0], &State)) {
        return enif_make_badarg(env);
    }
    if (State.size != DEC_STATE_LEN_WB) {
        return enif_make_badarg(env);
    }
    ErlNifBinary Frame;
    if (!enif_inspect_binary(env, argv[1], &Frame)) {
        return enif_make_badarg(env);
    }
    if (Frame.size > MAX_LEN_WB) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM NState;
    WB_dec_if_state* new_state = (WB_dec_if_state*)enif_make_new_binary(env, DEC_STATE_LEN_WB, &NState);
    memcpy(new_state, State.data, DEC_STATE_LEN_WB);

    ERL_NIF_TERM Pcm;
    int16_t* pcm = (int16_t*)enif_make_new_binary(env, SAMPLES_WB*sizeof(int16_t), &Pcm);

    D_IF_decode(new_state, Frame.data, pcm, 0);

    return enif_make_tuple2(env, NState, Pcm);
}


static ERL_NIF_TERM amrwb_encode_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    WB_enc_if_state* state = (WB_enc_if_state*)enif_make_new_binary(env, ENC_STATE_LEN_WB, &ret);
    E_IF_init(state);
    return ret;
}
static ERL_NIF_TERM amrwb_decode_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    WB_dec_if_state* state = (WB_dec_if_state*)enif_make_new_binary(env, DEC_STATE_LEN_WB, &ret);
    D_IF_init(state);
    return ret;
}

static ErlNifFunc nif_funcs[] =
{
  {"amrnb_encode_init", 0, amrnb_encode_init},
  {"amrnb_decode_init", 0, amrnb_decode_init},
  {"amrnb_encode_frame", 3, amrnb_encode_frame},
  {"amrnb_decode_frame", 2, amrnb_decode_frame},

  {"amrwb_encode_init", 0, amrwb_encode_init},
  {"amrwb_decode_init", 0, amrwb_decode_init},
  {"amrwb_encode_frame", 3, amrwb_encode_frame},
  {"amrwb_decode_frame", 2, amrwb_decode_frame}
};

ERL_NIF_INIT(amr_codec,nif_funcs,NULL,NULL,NULL,NULL)
