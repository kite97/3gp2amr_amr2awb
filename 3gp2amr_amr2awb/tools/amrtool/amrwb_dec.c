#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dec_if.h>

#define AMRWB_MAGIC_NUMBER "#!AMR-WB\n"

int main(int argc, char * argv[]) {
    FILE *f_serial; /* File of serial bits for transmission */
    FILE *f_synth;  /* File of speech data */
    Word16 synth[L_FRAME16k]; /* Buffer for speech @ 16kHz */
    UWord8 serial[NB_SERIAL_MAX];
    Word16 mode;

    short block_size[16]={18, 24, 33, 37, 41, 47, 51, 59, 61, 6, 6, 0, 0, 0, 1, 1};
    char magic[16];

    if (argc == 3)
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
        fprintf(stderr, "%s%s%s\n","Use: ",argv[0], " input.file output.file ");
        exit(0);
    }

    WB_dec_if_state destate;
    D_IF_init(&destate);
    fread(magic, sizeof(char), strlen(AMRWB_MAGIC_NUMBER), f_serial);
    if (strncmp(magic, AMRWB_MAGIC_NUMBER, strlen(AMRWB_MAGIC_NUMBER)))
    {
        fprintf(stderr, "%s%s\n", "Invalid magic number: ", magic);
        fclose(f_serial);
        fclose(f_synth);
        exit(0);
    }
    while (fread(serial, sizeof (UWord8), 1, f_serial) > 0)
    {
        mode = (Word16)((serial[0] >> 3) & 0x0F);
        fread(&serial[1], sizeof (UWord8), block_size[mode] - 1, f_serial );
        D_IF_decode(&destate, serial, synth, 0);
        fwrite(synth, sizeof(Word16), L_FRAME16k, f_synth);
        fflush(f_synth);
    }
    fclose(f_serial);
    fclose(f_synth);

    return 0;
}
