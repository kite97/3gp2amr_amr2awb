#include <stdint.h>
#include <math.h>
#include <erl_nif.h>

#define SAMPLE_IN_LEN 160

#define	MAX_DTMF_DIGITS		128

enum ast_frame_type {
    /*! DTMF end event, subclass is the digit */
    AST_FRAME_DTMF_END = 1,
    /*! Voice data, subclass is AST_FORMAT_* */
    AST_FRAME_VOICE,
    /*! Video frame, maybe?? :) */
    AST_FRAME_VIDEO,
    /*! A control frame, subclass is AST_CONTROL_* */
    AST_FRAME_CONTROL,
    /*! An empty, useless frame */
    AST_FRAME_NULL,
    /*! Inter Asterisk Exchange private frame type */
    AST_FRAME_IAX,
    /*! Text messages */
    AST_FRAME_TEXT,
    /*! Image Frames */
    AST_FRAME_IMAGE,
    /*! HTML Frame */
    AST_FRAME_HTML,
    /*! Comfort Noise frame (subclass is level of CNG in -dBov),
      body may include zero or more 8-bit quantization coefficients */
    AST_FRAME_CNG,
    /*! Modem-over-IP data streams */
    AST_FRAME_MODEM,
    /*! DTMF begin event, subclass is the digit */
    AST_FRAME_DTMF_BEGIN,
};

#define DTMF_THRESHOLD		8.0e7
#define DTMF_GSIZE		102
#define AST_FORMAT_ATTR_SIZE 64
#define AST_FORMAT_INC 100000
#define DTMF_TO_TOTAL_ENERGY	42.0
#define DTMF_RELATIVE_PEAK_ROW	6.3     /* 8dB */
#define DTMF_RELATIVE_PEAK_COL	6.3     /* 8dB */
#define M_PI       3.14159265358979323846
#define DSP_HISTORY		15
#define DEFAULT_SAMPLE_RATE		8000
#define	DSP_DIGITMODE_DTMF			0				/*!< Detect DTMF digits */
#define DSP_FEATURE_SILENCE_SUPPRESS	(1 << 0)
#define DEFAULT_THRESHOLD	512
#define DSP_DIGITMODE_NOQUELCH		(1 << 8)		/*!< Do not quelch DTMF from in-band */
#define DSP_DIGITMODE_RELAXDTMF		(1 << 11)		/*!< "Radio" mode (relaxed DTMF) */

/* \brief Format Categories*/
enum ast_format_type {
    AST_FORMAT_TYPE_AUDIO = 1 * AST_FORMAT_INC,
    AST_FORMAT_TYPE_VIDEO = 2 * AST_FORMAT_INC,
    AST_FORMAT_TYPE_IMAGE = 3 * AST_FORMAT_INC,
    AST_FORMAT_TYPE_TEXT  = 4 * AST_FORMAT_INC,
};

enum ast_format_id {
    /*! G.723.1 compression */
    AST_FORMAT_G723_1           = 1 + AST_FORMAT_TYPE_AUDIO,
    /*! GSM compression */
    AST_FORMAT_GSM              = 2 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw mu-law data (G.711) */
    AST_FORMAT_ULAW             = 3 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw A-law data (G.711) */
    AST_FORMAT_ALAW             = 4 + AST_FORMAT_TYPE_AUDIO,
    /*! ADPCM (G.726, 32kbps, AAL2 codeword packing) */
    AST_FORMAT_G726_AAL2        = 5 + AST_FORMAT_TYPE_AUDIO,
    /*! ADPCM (IMA) */
    AST_FORMAT_ADPCM            = 6 + AST_FORMAT_TYPE_AUDIO,
    /*! LPC10, 180 samples/frame */
    AST_FORMAT_LPC10            = 7 + AST_FORMAT_TYPE_AUDIO,
    /*! G.729A audio */
    AST_FORMAT_G729A            = 8 + AST_FORMAT_TYPE_AUDIO,
    /*! SpeeX Free Compression */
    AST_FORMAT_SPEEX            = 9 + AST_FORMAT_TYPE_AUDIO,
    /*! iLBC Free Compression */
    AST_FORMAT_ILBC             = 10 + AST_FORMAT_TYPE_AUDIO,
    /*! ADPCM (G.726, 32kbps, RFC3551 codeword packing) */
    AST_FORMAT_G726             = 11 + AST_FORMAT_TYPE_AUDIO,
    /*! G.722 */
    AST_FORMAT_G722             = 12 + AST_FORMAT_TYPE_AUDIO,
    /*! G.722.1 (also known as Siren7, 32kbps assumed) */
    AST_FORMAT_SIREN7           = 13 + AST_FORMAT_TYPE_AUDIO,
    /*! G.722.1 Annex C (also known as Siren14, 48kbps assumed) */
    AST_FORMAT_SIREN14          = 14 + AST_FORMAT_TYPE_AUDIO,
    /*! G.719 (64 kbps assumed) */
    AST_FORMAT_G719             = 15 + AST_FORMAT_TYPE_AUDIO,
    /*! SpeeX Wideband (16kHz) Free Compression */
    AST_FORMAT_SPEEX16          = 16 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw mu-law data (G.711) */
    AST_FORMAT_TESTLAW          = 17 + AST_FORMAT_TYPE_AUDIO,
    /*! SILK format */
    AST_FORMAT_SILK             = 18 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (8000 Hz) PCM */
    AST_FORMAT_SLINEAR          = 19 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (12000 Hz) PCM */
    AST_FORMAT_SLINEAR12        = 20 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (16000 Hz) PCM */
    AST_FORMAT_SLINEAR16        = 21 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (24000 Hz) PCM */
    AST_FORMAT_SLINEAR24        = 22 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (32000 Hz) PCM */
    AST_FORMAT_SLINEAR32        = 23 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (44100 Hz) PCM just because we can. */
    AST_FORMAT_SLINEAR44        = 24 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (48000 Hz) PCM */
    AST_FORMAT_SLINEAR48        = 25 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (96000 Hz) PCM */
    AST_FORMAT_SLINEAR96        = 26 + AST_FORMAT_TYPE_AUDIO,
    /*! Raw 16-bit Signed Linear (192000 Hz) PCM.  maybe we're taking this too far. */
    AST_FORMAT_SLINEAR192       = 27 + AST_FORMAT_TYPE_AUDIO,
    AST_FORMAT_SPEEX32          = 28 + AST_FORMAT_TYPE_AUDIO,
    AST_FORMAT_CELT             = 29 + AST_FORMAT_TYPE_AUDIO,

    /*! H.261 Video */
    AST_FORMAT_H261             = 1 + AST_FORMAT_TYPE_VIDEO,
    /*! H.263 Video */
    AST_FORMAT_H263             = 2 + AST_FORMAT_TYPE_VIDEO,
    /*! H.263+ Video */
    AST_FORMAT_H263_PLUS        = 3 + AST_FORMAT_TYPE_VIDEO,
    /*! H.264 Video */
    AST_FORMAT_H264             = 4 + AST_FORMAT_TYPE_VIDEO,
    /*! MPEG4 Video */
    AST_FORMAT_MP4_VIDEO        = 5 + AST_FORMAT_TYPE_VIDEO,

    /*! JPEG Images */
    AST_FORMAT_JPEG             = 1 + AST_FORMAT_TYPE_IMAGE,
    /*! PNG Images */
    AST_FORMAT_PNG              = 2 + AST_FORMAT_TYPE_IMAGE,

    /*! T.140 RED Text format RFC 4103 */
    AST_FORMAT_T140RED          = 1 + AST_FORMAT_TYPE_TEXT,
    /*! T.140 Text format - ITU T.140, RFC 4103 */
    AST_FORMAT_T140             = 2 + AST_FORMAT_TYPE_TEXT,
};

/*! Determine what type of media a ast_format_id is. */
#define AST_FORMAT_GET_TYPE(id) (((int) (id / AST_FORMAT_INC)) * AST_FORMAT_INC)


/*! \brief This structure contains the buffer used for format attributes */
struct ast_format_attr {
    /*! The buffer formats can use to represent attributes */
    uint32_t format_attr[AST_FORMAT_ATTR_SIZE];
    /*! If a format's payload needs to pass through that a new marker is required
     * for RTP, this variable will be set. */
    uint8_t rtp_marker_bit;
};

/*! \brief Represents a media format within Asterisk. */
struct ast_format {
    /*! The unique id representing this format from all the other formats. */
    enum ast_format_id id;
    /*!  Attribute structure used to associate attributes with a format. */
    struct ast_format_attr fattr;
};

union ast_frame_subclass {
    int integer;
    struct ast_format format;
};

struct ast_frame {
    /*! Kind of frame */
    enum ast_frame_type frametype;
    /*! Subclass, frame dependent */
    union ast_frame_subclass subclass;
    /*! Length of data */
    int datalen;
    /*! Number of samples in this frame */
    int samples;
    /*! Was the data malloc'd?  i.e. should we free it when we discard the frame? */
    int mallocd;
    /*! The number of bytes allocated for a malloc'd frame header */
    size_t mallocd_hdr_len;
    /*! How many bytes exist _before_ "data" that can be used if needed */
    int offset;
    /*! Optional source of frame for debugging */
    const char *src;
    /*! Pointer to actual data */
    union { void *ptr; uint32_t uint32; char pad[8]; } data;
    /*! Misc. frame flags */
    unsigned int flags;
    /*! Timestamp in milliseconds */
    long ts;
    /*! Length in milliseconds */
    long len;
    /*! Sequence number */
    int seqno;
};

typedef struct {
    int v2;
    int v3;
    int chunky;
    int fac;
    int samples;
} goertzel_state_t;

typedef struct {
    goertzel_state_t row_out[4];
    goertzel_state_t col_out[4];
    int hits;			/* How many successive hits we have seen already */
    int misses;			/* How many successive misses we have seen already */
    int lasthit;
    int current_hit;
    float energy;
    int current_sample;
    int mute_samples;
} dtmf_detect_state_t;

typedef struct {
    char digits[MAX_DTMF_DIGITS + 1];
    int digitlen[MAX_DTMF_DIGITS + 1];
    int current_digits;
    int detected_digits;
    int lost_digits;

    union {
        dtmf_detect_state_t dtmf;
    } td;
} digit_detect_state_t;

typedef struct {
    int start;
    int end;
} fragment_t;

typedef struct {
    int freq;
    int block_size;
    int squelch;		/* Remove (squelch) tone */
    goertzel_state_t tone;
    float energy;		/* Accumulated energy of the current block */
    int samples_pending;	/* Samples remain to complete the current block */
    int mute_samples;	/* How many additional samples needs to be muted to suppress already detected tone */

    int hits_required;	/* How many successive blocks with tone we are looking for */
    float threshold;	/* Energy of the tone relative to energy from all other signals to consider a hit */

    int hit_count;		/* How many successive blocks we consider tone present */
    int last_hit;		/* Indicates if the last processed block was a hit */

} tone_detect_state_t;

/*! Number of goertzels for progress detect */
enum gsamp_size {
    GSAMP_SIZE_NA = 183,			/*!< North America - 350, 440, 480, 620, 950, 1400, 1800 Hz */
    GSAMP_SIZE_CR = 188,			/*!< Costa Rica, Brazil - Only care about 425 Hz */
    GSAMP_SIZE_UK = 160			/*!< UK disconnect goertzel feed - should trigger 400hz */
};

enum prog_mode {
    PROG_MODE_NA = 0,
    PROG_MODE_CR,
    PROG_MODE_UK
};

struct ast_dsp_busy_pattern {
    /*! Number of elements. */
    int length;
    /*! Pattern elements in on/off time durations. */
    int pattern[4];
};

struct ast_dsp {
    struct ast_frame f;
    int threshold;
    int totalsilence;
    int totalnoise;
    int features;
    int ringtimeout;
    int busymaybe;
    int busycount;
    struct ast_dsp_busy_pattern busy_cadence;
    int historicnoise[DSP_HISTORY];
    int historicsilence[DSP_HISTORY];
    goertzel_state_t freqs[7];
    int freqcount;
    int gsamps;
    enum gsamp_size gsamp_size;
    enum prog_mode progmode;
    int tstate;
    int tcount;
    int digitmode;
    int faxmode;
    int dtmf_began;
    int display_inband_dtmf_warning;
    float genergy;
    int mute_fragments;
    unsigned int sample_rate;
    fragment_t mute_data[5];
    digit_detect_state_t digit_state;
    tone_detect_state_t cng_tone_state;
    tone_detect_state_t ced_tone_state;
};

typedef struct {
    int value;
    int power;
} goertzel_result_t;

static const float dtmf_row[] = {
    697.0,  770.0,  852.0,  941.0
};
static const float dtmf_col[] = {
    1209.0, 1336.0, 1477.0, 1633.0
};
//static const float mf_tones[] = {
//	700.0, 900.0, 1100.0, 1300.0, 1500.0, 1700.0
//};
enum threshold {
    /* Array offsets */
    THRESHOLD_SILENCE = 0,
    /* Always the last */
    THRESHOLD_MAX = 1,
};
static const char dtmf_positions[] = "123A" "456B" "789C" "*0#D";
//static const char bell_mf_positions[] = "1247C-358A--69*---0B----#";
//static float dtmf_normal_twist;		/* AT&T = 8dB */
//static float dtmf_reverse_twist;	/* AT&T = 4dB */
//static float relax_dtmf_normal_twist;	/* AT&T = 8dB */
//static float relax_dtmf_reverse_twist;	/* AT&T = 6dB */
//static int dtmf_hits_to_begin;		/* How many successive hits needed to consider begin of a digit */
//static int dtmf_misses_to_end;		/* How many successive misses needed to consider end of a digit */
#define DEF_DTMF_NORMAL_TWIST		6.31	 /* 8.0dB */
#define DEF_RELAX_DTMF_NORMAL_TWIST	6.31	 /* 8.0dB */
#ifdef	RADIO_RELAX
#define DEF_DTMF_REVERSE_TWIST		2.51	 /* 4.01dB */
#define DEF_RELAX_DTMF_REVERSE_TWIST	6.61	 /* 8.2dB */
#else
#define DEF_DTMF_REVERSE_TWIST		2.51	 /* 4.01dB */
#define DEF_RELAX_DTMF_REVERSE_TWIST	3.98	 /* 6.0dB */
#endif

/* How many successive hits needed to consider begin of a digit
 * IE. Override with dtmf_hits_to_begin=4 in dsp.conf
 */
#define DEF_DTMF_HITS_TO_BEGIN	2

/* How many successive misses needed to consider end of a digit
 * IE. Override with dtmf_misses_to_end=4 in dsp.conf
 */
#define DEF_DTMF_MISSES_TO_END	3
static float dtmf_normal_twist = DEF_DTMF_NORMAL_TWIST;
static float dtmf_reverse_twist = DEF_DTMF_REVERSE_TWIST;
static float relax_dtmf_normal_twist = DEF_RELAX_DTMF_NORMAL_TWIST;
static float relax_dtmf_reverse_twist = DEF_RELAX_DTMF_REVERSE_TWIST;
static int dtmf_hits_to_begin = DEF_DTMF_HITS_TO_BEGIN;
static int dtmf_misses_to_end = DEF_DTMF_MISSES_TO_END;


static inline void goertzel_sample(goertzel_state_t *s, short sample) {
    int v1;

    v1 = s->v2;
    s->v2 = s->v3;

    s->v3 = (s->fac * s->v2) >> 15;
    s->v3 = s->v3 - v1 + (sample >> s->chunky);
    if (abs(s->v3) > 32768) {
        s->chunky++;
        s->v3 = s->v3 >> 1;
        s->v2 = s->v2 >> 1;
    }
}

static inline void goertzel_init(goertzel_state_t *s, float freq, int samples, unsigned int sample_rate) {
    s->v2 = s->v3 = s->chunky = 0.0;
    s->fac = (int)(32768.0 * 2.0 * cos(2.0 * M_PI * freq / sample_rate));
    s->samples = samples;
}

static void ast_dtmf_detect_init (dtmf_detect_state_t *s, unsigned int sample_rate) {
    int i;

    for (i = 0;  i < 4;  i++) {
        goertzel_init(&s->row_out[i], dtmf_row[i], DTMF_GSIZE, sample_rate);
        goertzel_init(&s->col_out[i], dtmf_col[i], DTMF_GSIZE, sample_rate);
    }
    s->lasthit = 0;
    s->current_hit = 0;
    s->energy = 0.0;
    s->current_sample = 0;
    s->hits = 0;
    s->misses = 0;
    s->mute_samples = 0;
}

static void ast_digit_detect_init(digit_detect_state_t *s, unsigned int sample_rate) {
    s->current_digits = 0;
    s->detected_digits = 0;
    s->lost_digits = 0;
    s->digits[0] = '\0';

    ast_dtmf_detect_init(&s->td.dtmf, sample_rate);
}

static inline void goertzel_reset(goertzel_state_t *s) {
    s->v2 = s->v3 = s->chunky = 0.0;
}

#define ARRAY_LEN(a) (size_t) (sizeof(a) / sizeof(0[a]))

static void mute_fragment(struct ast_dsp *dsp, fragment_t *fragment) {
    if (dsp->mute_fragments >= ARRAY_LEN(dsp->mute_data)) {
        return;
    }

    dsp->mute_data[dsp->mute_fragments++] = *fragment;
}

static inline float goertzel_result(goertzel_state_t *s) {
    goertzel_result_t r;
    r.value = (s->v3 * s->v3) + (s->v2 * s->v2);
    r.value -= ((s->v2 * s->v3) >> 15) * s->fac;
    r.power = s->chunky * 2;
    return (float)r.value * (float)(1 << r.power);
}

static void store_digit(digit_detect_state_t *s, char digit) {
    s->detected_digits++;
    if (s->current_digits < MAX_DTMF_DIGITS) {
        s->digitlen[s->current_digits] = 0;
        s->digits[s->current_digits++] = digit;
        s->digits[s->current_digits] = '\0';
    } else {
        s->lost_digits++;
    }
}

static int dtmf_detect(struct ast_dsp *dsp, digit_detect_state_t *s, const int16_t amp[], int samples, int squelch, int relax) {
    float row_energy[4];
    float col_energy[4];
    int i;
    int j;
    int sample;
    short samp;
    int best_row;
    int best_col;
    int hit;
    int limit;
    fragment_t mute = {0, 0};

    if (squelch && s->td.dtmf.mute_samples > 0) {
        mute.end = (s->td.dtmf.mute_samples < samples) ? s->td.dtmf.mute_samples : samples;
        s->td.dtmf.mute_samples -= mute.end;
    }

    hit = 0;
    for (sample = 0; sample < samples; sample = limit) {
        /* DTMF_GSIZE is optimised to meet the DTMF specs. */
        if ((samples - sample) >= (DTMF_GSIZE - s->td.dtmf.current_sample)) {
            limit = sample + (DTMF_GSIZE - s->td.dtmf.current_sample);
        } else {
            limit = samples;
        }
        /* The following unrolled loop takes only 35% (rough estimate) of the
           time of a rolled loop on the machine on which it was developed */
        for (j = sample; j < limit; j++) {
            samp = amp[j];
            s->td.dtmf.energy += (int32_t) samp * (int32_t) samp;
            /* With GCC 2.95, the following unrolled code seems to take about 35%
               (rough estimate) as long as a neat little 0-3 loop */
            goertzel_sample(s->td.dtmf.row_out, samp);
            goertzel_sample(s->td.dtmf.col_out, samp);
            goertzel_sample(s->td.dtmf.row_out + 1, samp);
            goertzel_sample(s->td.dtmf.col_out + 1, samp);
            goertzel_sample(s->td.dtmf.row_out + 2, samp);
            goertzel_sample(s->td.dtmf.col_out + 2, samp);
            goertzel_sample(s->td.dtmf.row_out + 3, samp);
            goertzel_sample(s->td.dtmf.col_out + 3, samp);
        }
        s->td.dtmf.current_sample += (limit - sample);
        if (s->td.dtmf.current_sample < DTMF_GSIZE) {
            continue;
        }
        /* We are at the end of a DTMF detection block */
        /* Find the peak row and the peak column */
        row_energy[0] = goertzel_result (&s->td.dtmf.row_out[0]);
        col_energy[0] = goertzel_result (&s->td.dtmf.col_out[0]);

        for (best_row = best_col = 0, i = 1;  i < 4;  i++) {
            row_energy[i] = goertzel_result (&s->td.dtmf.row_out[i]);
            if (row_energy[i] > row_energy[best_row]) {
                best_row = i;
            }
            col_energy[i] = goertzel_result (&s->td.dtmf.col_out[i]);
            if (col_energy[i] > col_energy[best_col]) {
                best_col = i;
            }
        }
        hit = 0;
        /* Basic signal level test and the twist test */
        if (row_energy[best_row] >= DTMF_THRESHOLD &&
                col_energy[best_col] >= DTMF_THRESHOLD &&
                col_energy[best_col] < row_energy[best_row] * (relax ? relax_dtmf_reverse_twist : dtmf_reverse_twist) &&
                row_energy[best_row] < col_energy[best_col] * (relax ? relax_dtmf_normal_twist : dtmf_normal_twist)) {
            /* Relative peak test */
            for (i = 0;  i < 4;  i++) {
                if ((i != best_col &&
                            col_energy[i] * DTMF_RELATIVE_PEAK_COL > col_energy[best_col]) ||
                        (i != best_row
                         && row_energy[i] * DTMF_RELATIVE_PEAK_ROW > row_energy[best_row])) {
                    break;
                }
            }
            /* ... and fraction of total energy test */
            if (i >= 4 &&
                    (row_energy[best_row] + col_energy[best_col]) > DTMF_TO_TOTAL_ENERGY * s->td.dtmf.energy) {
                /* Got a hit */
                hit = dtmf_positions[(best_row << 2) + best_col];
            }
        }

        /*
         * Adapted from ETSI ES 201 235-3 V1.3.1 (2006-03)
         * (40ms reference is tunable with hits_to_begin and misses_to_end)
         * each hit/miss is 12.75ms with DTMF_GSIZE at 102
         *
         * Character recognition: When not DRC *(1) and then
         *      Shall exist VSC > 40 ms (hits_to_begin)
         *      May exist 20 ms <= VSC <= 40 ms
         *      Shall not exist VSC < 20 ms
         *
         * Character recognition: When DRC and then
         *      Shall cease Not VSC > 40 ms (misses_to_end)
         *      May cease 20 ms >= Not VSC >= 40 ms
         *      Shall not cease Not VSC < 20 ms
         *
         * *(1) or optionally a different digit recognition condition
         *
         * Legend: VSC The continuous existence of a valid signal condition.
         *      Not VSC The continuous non-existence of valid signal condition.
         *      DRC The existence of digit recognition condition.
         *      Not DRC The non-existence of digit recognition condition.
         */

        /*
         * Example: hits_to_begin=2 misses_to_end=3
         * -------A last_hit=A hits=0&1
         * ------AA hits=2 current_hit=A misses=0       BEGIN A
         * -----AA- misses=1 last_hit=' ' hits=0
         * ----AA-- misses=2
         * ---AA--- misses=3 current_hit=' '            END A
         * --AA---B last_hit=B hits=0&1
         * -AA---BC last_hit=C hits=0&1
         * AA---BCC hits=2 current_hit=C misses=0       BEGIN C
         * A---BCC- misses=1 last_hit=' ' hits=0
         * ---BCC-C misses=0 last_hit=C hits=0&1
         * --BCC-CC misses=0
         *
         * Example: hits_to_begin=3 misses_to_end=2
         * -------A last_hit=A hits=0&1
         * ------AA hits=2
         * -----AAA hits=3 current_hit=A misses=0       BEGIN A
         * ----AAAB misses=1 last_hit=B hits=0&1
         * ---AAABB misses=2 current_hit=' ' hits=2     END A
         * --AAABBB hits=3 current_hit=B misses=0       BEGIN B
         * -AAABBBB misses=0
         *
         * Example: hits_to_begin=2 misses_to_end=2
         * -------A last_hit=A hits=0&1
         * ------AA hits=2 current_hit=A misses=0       BEGIN A
         * -----AAB misses=1 hits=0&1
         * ----AABB misses=2 current_hit=' ' hits=2 current_hit=B misses=0 BEGIN B
         * ---AABBB misses=0
         */

        if (s->td.dtmf.current_hit) {
            /* We are in the middle of a digit already */
            if (hit != s->td.dtmf.current_hit) {
                s->td.dtmf.misses++;
                if (s->td.dtmf.misses == dtmf_misses_to_end) {
                    /* There were enough misses to consider digit ended */
                    s->td.dtmf.current_hit = 0;
                }
            } else {
                s->td.dtmf.misses = 0;
                /* Current hit was same as last, so increment digit duration (of last digit) */
                s->digitlen[s->current_digits - 1] += DTMF_GSIZE;
            }
        }

        /* Look for a start of a new digit no matter if we are already in the middle of some
           digit or not. This is because hits_to_begin may be smaller than misses_to_end
           and we may find begin of new digit before we consider last one ended. */

        if (hit != s->td.dtmf.lasthit) {
            s->td.dtmf.lasthit = hit;
            s->td.dtmf.hits = 0;
        }
        if (hit && hit != s->td.dtmf.current_hit) {
            s->td.dtmf.hits++;
            if (s->td.dtmf.hits == dtmf_hits_to_begin) {
                store_digit(s, hit);
                s->digitlen[s->current_digits - 1] = dtmf_hits_to_begin * DTMF_GSIZE;
                s->td.dtmf.current_hit = hit;
                s->td.dtmf.misses = 0;
            }
        }

        /* If we had a hit in this block, include it into mute fragment */
        if (squelch && hit) {
            if (mute.end < sample - DTMF_GSIZE) {
                /* There is a gap between fragments */
                mute_fragment(dsp, &mute);
                mute.start = (sample > DTMF_GSIZE) ? (sample - DTMF_GSIZE) : 0;
            }
            mute.end = limit + DTMF_GSIZE;
        }

        /* Reinitialise the detector for the next block */
        for (i = 0; i < 4; i++) {
            goertzel_reset(&s->td.dtmf.row_out[i]);
            goertzel_reset(&s->td.dtmf.col_out[i]);
        }
        s->td.dtmf.energy = 0.0;
        s->td.dtmf.current_sample = 0;
    }

    if (squelch && mute.end) {
        if (mute.end > samples) {
            s->td.dtmf.mute_samples = mute.end - samples;
            mute.end = samples;
        }
        mute_fragment(dsp, &mute);
    }

    return (s->td.dtmf.current_hit);	/* return the debounced hit */
}

static struct progress {
    enum gsamp_size size;
    int freqs[7];
} modes[] = {
    { GSAMP_SIZE_NA, { 350, 440, 480, 620, 950, 1400, 1800 } },	/*!< North America */
    { GSAMP_SIZE_CR, { 425 } },					/*!< Costa Rica, Brazil */
    { GSAMP_SIZE_UK, { 350, 400, 440 } },				/*!< UK */
};

static void ast_dsp_prog_reset(struct ast_dsp *dsp) {
    int max = 0;
    int x;

    dsp->gsamp_size = modes[dsp->progmode].size;
    dsp->gsamps = 0;
    for (x = 0; x < ARRAY_LEN(modes[dsp->progmode].freqs); x++) {
        if (modes[dsp->progmode].freqs[x]) {
            goertzel_init(&dsp->freqs[x], (float)modes[dsp->progmode].freqs[x], dsp->gsamp_size, dsp->sample_rate);
            max = x + 1;
        }
    }
    dsp->freqcount = max;
    dsp->ringtimeout= 0;
}

static int ast_dsp_init(struct ast_dsp *dsp) {

    dsp->threshold = DEFAULT_THRESHOLD;
    dsp->features = DSP_FEATURE_SILENCE_SUPPRESS;
    dsp->busycount = DSP_HISTORY;
    dsp->digitmode = DSP_DIGITMODE_DTMF;
    dsp->sample_rate = DEFAULT_SAMPLE_RATE;
    /* Initialize digit detector */
    ast_digit_detect_init(&dsp->digit_state, dsp->sample_rate);
    dsp->display_inband_dtmf_warning = 1;
    /* Initialize initial DSP progress detect parameters */
    dsp->progmode = PROG_MODE_NA;
    ast_dsp_prog_reset(dsp);
    return 0;
}

static ERL_NIF_TERM dtmf_hint(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary orig_dsp;
    if (!enif_inspect_binary(env, argv[0], &orig_dsp)) {
        return enif_make_badarg(env);
    }
    if (orig_dsp.size != sizeof(struct ast_dsp)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary orig_bin;
    if (!enif_inspect_binary(env, argv[1], &orig_bin)) {
        return enif_make_badarg(env);
    }
    //数据长度应该满足某些要求，不能过长也不能过短
    //暂时严格要求
    if (orig_bin.size != SAMPLE_IN_LEN*2) {
        return enif_make_badarg(env);
    }
    const int16_t* data = (int16_t*)orig_bin.data;
    const struct ast_dsp* p_dsp = (struct ast_dsp *)orig_dsp.data;
    struct ast_dsp dsp = *p_dsp;
    int digit = 0;
    digit = dtmf_detect(&dsp, &dsp.digit_state, data, SAMPLE_IN_LEN, (dsp.digitmode & DSP_DIGITMODE_NOQUELCH) == 0, (dsp.digitmode & DSP_DIGITMODE_RELAXDTMF));
    ERL_NIF_TERM new_dsp_term;
    struct ast_dsp* new_dsp = (struct ast_dsp*)(enif_make_new_binary(env, sizeof(struct ast_dsp), &new_dsp_term));
    *new_dsp = dsp;
    return enif_make_tuple2(env, new_dsp_term,
            enif_make_int(env, digit));
}
static ERL_NIF_TERM init_dsp(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    struct ast_dsp *dsp = (struct ast_dsp *)(enif_make_new_binary(env, sizeof(struct ast_dsp), &ret));
    ast_dsp_init(dsp);
    return ret;
}

static ErlNifFunc nif_funcs[] = {
    {"parse", 2, dtmf_hint},
    {"init_dsp", 0, init_dsp}
};

ERL_NIF_INIT(dtmf,nif_funcs,NULL,NULL,NULL,NULL)
