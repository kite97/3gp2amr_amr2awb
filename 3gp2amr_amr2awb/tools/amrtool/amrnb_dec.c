#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interf_dec.h>

#define AMR_MAGIC_NUMBER "#!AMR\n"

int main(int argc, char * argv[]) {
    FILE * file_speech = NULL;
    FILE * file_analysis = NULL;
    short synth[160];
    int read_size;
    unsigned char analysis[32];
    enum Mode dec_mode;

    short block_size[16]={ 12, 13, 15, 17, 19, 20, 26, 31, 5, 0, 0, 0, 0, 0, 0, 0 };
    char magic[8];

    if (argc == 3)
    {
        if ((file_speech = fopen(argv[2], "wb")) == NULL)
        {
            exit(0);
        }
        if ((file_analysis = fopen(argv[1], "rb")) == NULL)
        {
            exit(0);
        }
    }
    else
    {
        fprintf(stderr, "%s%s%s\n","Use: ",argv[0], " input.file output.file ");
        exit(0);
    }

    dec_interface_State destate;
    Decoder_Interface_init(&destate);
    fread(magic, sizeof(char), strlen(AMR_MAGIC_NUMBER), file_analysis);
    if (strncmp(magic, AMR_MAGIC_NUMBER, strlen(AMR_MAGIC_NUMBER)))
    {
        fprintf(stderr, "%s%s\n", "Invalid magic number: ", magic);
        fclose(file_speech);
        fclose(file_analysis);
        exit(0);
    }
    while (fread(analysis, sizeof (unsigned char), 1, file_analysis ) > 0)
    {
        dec_mode = (analysis[0] >> 3) & 0x000F;
        read_size = block_size[dec_mode];
        fread(&analysis[1], sizeof (char), read_size, file_analysis);
        Decoder_Interface_Decode(&destate, analysis, synth, 0);
        fwrite(synth, sizeof (short), 160, file_speech);
    }
    fclose(file_speech);
    fclose(file_analysis);

    return 0;
}
