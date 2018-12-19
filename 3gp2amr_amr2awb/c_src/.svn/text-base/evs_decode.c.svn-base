#include <stdint.h>
#include <string.h>
#include <erl_nif.h>
#include <prot.h>
#include <mime.h>
#include <rom_com.h>

#define DEC_STATE_LEN_LOCAL sizeof(Decoder_State)

static ERL_NIF_TERM evs_decode_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    Decoder_State* st = (Decoder_State*)enif_make_new_binary(env, DEC_STATE_LEN_LOCAL, &ret);

    st->Opt_AMR_WB = 0;
    st->Opt_VOIP = 0;
    st->bitstreamformat = MIME;
    st->amrwb_rfc4867_flag = 0;

    st->output_Fs = 8000;

    init_decoder(st);
    reset_indices_dec(st);

    return ret;
}

static ERL_NIF_TERM evs_decode_frame(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary State;
    if (!enif_inspect_binary(env, argv[0], &State)) {
        return enif_make_badarg(env);
    }
    if (State.size != DEC_STATE_LEN_LOCAL) {
        return enif_make_badarg(env);
    }
    ErlNifBinary Frame;
    if (!enif_inspect_binary(env, argv[1], &Frame)) {
        return enif_make_badarg(env);
    }
    //if (Frame.size > MAX_LEN_NB) {
    //    return enif_make_badarg(env);
    //}

    ERL_NIF_TERM NState;
    Decoder_State* st = (Decoder_State*)enif_make_new_binary(env, DEC_STATE_LEN_LOCAL, &NState);
    memcpy(st, State.data, DEC_STATE_LEN_LOCAL);

    ERL_NIF_TERM Pcm;
    int output_frame = (short)(st->output_Fs / 50);
    int16_t* pcm = (int16_t*)enif_make_new_binary(env, output_frame*sizeof(int16_t), &Pcm);
    float output[L_FRAME48k];

    UWord8 header = ((UWord8*)Frame.data)[0];
    UWord8* frame_body = &((UWord8*)Frame.data)[1];

    update_state(st, header, frame_body);
    /* run the main decoding routine */
    if ( st->codec_mode == MODE1 )
    {
        if ( st->Opt_AMR_WB )
        {
            amr_wb_dec( st, output );
        }
        else
        {
            evs_dec( st, output, FRAMEMODE_NORMAL );
        }
    }
    else
    {
        if( !st->bfi )
        {
            evs_dec( st, output, FRAMEMODE_NORMAL );
        }
        else
        {
            evs_dec( st, output, FRAMEMODE_MISSING );
        }
    }

    /* convert 'float' output data to 'short' */
    syn_output( output, output_frame, pcm);
    /* increase the counter of initialization frames */
    if( st->ini_frame < MAX_FRAME_COUNTER )
    {
        st->ini_frame++;
    }


    return enif_make_tuple2(env, NState, Pcm);
}


static ErlNifFunc nif_funcs[] =
{
  {"init", 0, evs_decode_init},
  {"decode_frame", 2, evs_decode_frame}
};

ERL_NIF_INIT(evs_decode,nif_funcs,NULL,NULL,NULL,NULL)
