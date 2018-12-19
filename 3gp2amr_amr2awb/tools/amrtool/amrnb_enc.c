#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <typedef.h>
#include <interf_enc.h>

#define AMR_MAGIC_NUMBER "#!AMR\n"

int main(int argc, char * argv[]) {
    FILE * file_speech = NULL;
    FILE * file_encoded = NULL;
    short speech[160];
    int byte_counter, bytes = 0;
    int req_mode = 0;
    int dtx = 0;
    unsigned char serial_data[32];

    if (argc == 4)
    {
        if ((file_speech = fopen(argv[1], "rb"))== NULL)
        {
            exit(0);
        }
        if ((file_encoded = fopen(argv[2], "wb")) == NULL)
        {
            exit(0);
        }
        req_mode = atoi(argv[3]);
    }
    else
    {
        fprintf(stderr, "%s%s%s\n","Use: ",argv[0], " input.file output.file mode");
        exit(0);
    }

    enc_interface_State enstate;
    Encoder_Interface_init(&enstate, dtx);

    bytes = fwrite(AMR_MAGIC_NUMBER, sizeof(char), strlen(AMR_MAGIC_NUMBER), file_encoded);
    while (fread(speech, sizeof (Word16), 160, file_speech) > 0)
    {
       byte_counter = Encoder_Interface_Encode(&enstate, req_mode, speech, serial_data, 0);
       bytes += byte_counter;
       fwrite(serial_data, sizeof (UWord8), byte_counter, file_encoded);
       fflush(file_encoded);
    }
    return 0;
}
