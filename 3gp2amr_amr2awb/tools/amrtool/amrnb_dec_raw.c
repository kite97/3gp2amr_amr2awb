#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interf_dec.h>

#define OCT "1"
#define EFF "0"

int main(int argc, char * argv[]) {
    FILE * file_speech = NULL;
    FILE * file_analysis = NULL;
    short synth[160];
    int read_size;
    unsigned char analysis[32];
    unsigned char silence[160];
    enum Mode dec_mode;
    short block_size_oct[16]={ 12, 13, 15, 17, 19, 20, 26, 31, 5, 0, 0, 0, 0, 0, 0, 0 };
    short block_size_eff[16]={ 12, 13, 14, 16, 18, 20, 25, 30, 5, 0, 0, 0, 0, 0, 0, 0 };

    if (argc == 4)
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
    if (strncmp(argv[3], OCT, 1) == 0)
    {
        while (fread(analysis, sizeof (unsigned char), 1, file_analysis) > 0)
        {
            fread(analysis, sizeof (unsigned char), 1, file_analysis);
            dec_mode = (analysis[0] >> 3) & 0x000F;
            fread(&analysis[1], sizeof (char), block_size_oct[dec_mode], file_analysis);
            Decoder_Interface_Decode(&destate, analysis, synth, 0);
            fwrite(synth, sizeof (short), 160, file_speech);
        }
    }
    else if (strncmp(argv[3], EFF, 1) == 0)
    {
        while (fread(analysis, sizeof (unsigned char), 2, file_analysis) > 0)
        {
            if (analysis[0] == 0 && analysis[1] == 0)
            {
                fread(silence, sizeof(unsigned char), 158, file_analysis);
                for (int i = 0; i < 160; ++i)
                    synth[i] = 0xff;
                fwrite(synth, sizeof (short), 160, file_speech);
                continue;
            }
            dec_mode = ((analysis[0] & 0x07) << 1) + (analysis[1] >> 7);
            read_size = block_size_eff[dec_mode];
            //printf("analysis[0]=%x, analysis[1]=%x, mode-set=%d, read_size=%d\n", analysis[0], analysis[1], dec_mode, read_size);
            analysis[0] = (dec_mode << 3) + 4;
            fread(&analysis[2], sizeof(unsigned char), read_size , file_analysis);
            for (int i = 1; i < read_size; ++i)
            {
                analysis[i] = (analysis[i] << 2) + (analysis[i+1] >> 6);
            }
            analysis[read_size] = analysis[read_size] << 2;
            Decoder_Interface_Decode(&destate, analysis, synth, 0);
            fwrite(synth, sizeof (short), 160, file_speech);
        }
    }
    fclose(file_speech);
    fclose(file_analysis);

    return 0;
}
