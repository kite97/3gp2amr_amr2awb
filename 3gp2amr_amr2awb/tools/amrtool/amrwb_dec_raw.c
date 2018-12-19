#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dec_if.h>

#define OCT "1"
#define EFF "0"

int main(int argc, char * argv[]) {
    FILE *f_serial; /* File of serial bits for transmission */
    FILE *f_synth;  /* File of speech data */
    Word16 synth[L_FRAME16k]; /* Buffer for speech @ 16kHz */
    UWord8 serial[NB_SERIAL_MAX];
    Word16 mode;
    int read_size;
    short block_size[16]={18, 24, 33, 37, 41, 47, 51, 59, 61, 6, 6, 0, 0, 0, 1, 1};

    if (argc == 4)
    {
        if ((f_serial = fopen(argv[1], "rb")) == NULL)
        {
            exit(0);
        }

        if ((f_synth = fopen(argv[2], "wb")) == NULL)
        {
            exit(0);
        }
    }
    else
    {
        fprintf(stderr, "%s%s%s\n","Use: ",argv[0], " input.file output.file octet-align");
        exit(0);
    }

    WB_dec_if_state destate;
    D_IF_init(&destate);
    if (strncmp(argv[3], OCT, 1) == 0)
    {
        while (fread(serial, sizeof(UWord8), 1, f_serial) > 0)
        {
            fread(serial, sizeof (UWord8), 1, f_serial);
            mode = (Word16)((serial[0] >> 3) & 0x0F);
            fread(&serial[1], sizeof(UWord8), block_size[mode] - 1, f_serial);
            D_IF_decode(&destate, serial, synth, 0);
            fwrite(synth, sizeof(Word16), L_FRAME16k, f_synth);
            fflush(f_synth);
        }
    }
    else if (strncmp(argv[3], EFF, 1) == 0)
    {
        while (fread(serial, sizeof(UWord8), 2, f_serial) > 0)
        {
            mode = (Word16)(((serial[0] & 0x07) << 1) + (serial[1] >> 7));
            serial[0] = (UWord8)((mode << 3) + 4);
            read_size = block_size[mode];
            fread(&serial[2], sizeof(UWord8), read_size - 2, f_serial);
            for (int i = 1; i < read_size - 1; ++i)
            {
                serial[i] = (serial[i] << 2) + (serial[i+1] >> 6);
            }
            serial[read_size-1] = serial[read_size-1] << 2;
            D_IF_decode(&destate, serial, synth, 0);
            fwrite(synth, sizeof(Word16), L_FRAME16k, f_synth);
            fflush(f_synth);
        }
    }
    fclose(f_serial);
    fclose(f_synth);

    return 0;
}
