#include <stdint.h>
#include <string.h>
#include <erl_nif.h>
#include <prot.h>
#include <mime.h>
#include <rom_com.h>

#define ENC_STATE_LEN_LOCAL sizeof(Encoder_State)


static ERL_NIF_TERM evs_encode_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    int mode;

    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }
    Encoder_State* st = (Encoder_State*)enif_make_new_binary(env, ENC_STATE_LEN_LOCAL, &ret);
    memset(st, 0, ENC_STATE_LEN_LOCAL);

    io_ini_enc_clean(st, mode);
    init_encoder( st );
    return ret;
}

static ERL_NIF_TERM evs_encode_frame(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary State;
    if (!enif_inspect_binary(env, argv[0], &State)) {
        return enif_make_badarg(env);
    }
    if (State.size != ENC_STATE_LEN_LOCAL) {
        return enif_make_badarg(env);
    }
    ErlNifBinary Pcm;
    if (!enif_inspect_binary(env, argv[1], &Pcm)) {
        return enif_make_badarg(env);
    }

    Word16 pFrame_size = 0;
    UWord8 pFrame[((MAX_BITS_PER_FRAME + 7) >> 3)+1];
    int n_samples = Pcm.size / sizeof(int16_t);

    ERL_NIF_TERM NState;
    Encoder_State* st = (Encoder_State*)enif_make_new_binary(env, ENC_STATE_LEN_LOCAL, &NState);
    memcpy(st, State.data, ENC_STATE_LEN_LOCAL);

    short Opt_RF_ON_loc, rf_fec_offset_loc;
    Opt_RF_ON_loc = st->Opt_RF_ON;
    rf_fec_offset_loc = st->rf_fec_offset;

    if( ( st->Opt_RF_ON && ( st->total_brate != ACELP_13k20 ||  st->input_Fs == 8000 || st->max_bwidth == NB ) ) || st->rf_fec_offset == 0 )
    {
        if( st->total_brate == ACELP_13k20 )
        {
            st->codec_mode = MODE1;
            reset_rf_indices(st);
        }
        st->Opt_RF_ON = 0;
        st->rf_fec_offset = 0;
    }

    if( Opt_RF_ON_loc && rf_fec_offset_loc != 0 && L_sub( st->total_brate, ACELP_13k20 ) == 0 && L_sub( st->input_Fs, 8000 ) != 0 && st->max_bwidth != NB )
    {
        st->codec_mode = MODE2;
        if(st->Opt_RF_ON == 0)
        {
            reset_rf_indices(st);
        }
        st->Opt_RF_ON = 1;
        st->rf_fec_offset = rf_fec_offset_loc;
    }

    if ( ((st->input_Fs == 8000)|| (st->max_bwidth == NB)) && (st->total_brate > ACELP_24k40) )
    {
        st->total_brate = ACELP_24k40;
        st->codec_mode = MODE2;
    }

    if ( st->Opt_AMR_WB )
    {
        amr_wb_enc( st, (short int *)Pcm.data, n_samples );
    }
    else
    {
        evs_enc( st, (short int *)Pcm.data, n_samples );
    }

    indices_to_serial( st, pFrame, &pFrame_size );

    ERL_NIF_TERM Frame;
    UWord8* frame = (UWord8*)enif_make_new_binary(env, (((pFrame_size+7)>>3)+1)*sizeof(UWord8), &Frame);
    write_indices_to_buffer(st, frame, pFrame, pFrame_size);
    return enif_make_tuple2(env, NState, Frame);
}


static ErlNifFunc nif_funcs[] =
{
  {"init", 1, evs_encode_init},
  {"encode_frame", 2, evs_encode_frame}
};

ERL_NIF_INIT(evs_encode,nif_funcs,NULL,NULL,NULL,NULL)
