#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <enc_if.h>

#define AMRWB_MAGIC_NUMBER "#!AMR-WB\n"

int main(int argc, char * argv[]) {
    FILE *f_speech = NULL; /* File of speech data */
    FILE *f_serial = NULL; /* File of serial bits for transmission */
    Word32 serial_size;
    Word16 signal[L_FRAME16k]; /* Buffer for speech @ 16kHz */
    Word16 coding_mode = 0;
    UWord8 serial[NB_SERIAL_MAX];

    if (argc == 4)
    {
        if ((f_speech = fopen(argv[1], "rb")) == NULL)
        {
           exit(0);
        }
        if ((f_serial = fopen(argv[2], "wb")) == NULL)
        {
           exit(0);
        }
        coding_mode = atoi(argv[3]);
    }
    else
    {
        fprintf(stderr, "%s%s%s\n","Use: ",argv[0], " input.file output.file mode");
        exit(0);
    }

    WB_enc_if_state enstate;
    E_IF_init(&enstate);

    fwrite(AMRWB_MAGIC_NUMBER, sizeof(char), strlen(AMRWB_MAGIC_NUMBER), f_serial);
    while (fread(signal, sizeof(Word16), L_FRAME16k, f_speech) == L_FRAME16k)
    {
       serial_size = E_IF_encode(&enstate, coding_mode, signal, serial, 0);
       fwrite(serial, 1, serial_size, f_serial);
    }
    fclose(f_speech);
    fclose(f_serial);

    return 0;
}
