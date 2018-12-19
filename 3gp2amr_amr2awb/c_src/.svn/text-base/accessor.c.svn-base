#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>

#define MAX_PATH_LEN 256   //include tail null
#define BUFFER_LEN (1024*1024)

#define SUCC_NO_UPDATE 1
#define SUCC_UPDATE 2

#define ERR_MAX_LEN 100
#define ERR_REMOTE 101
#define ERR_LOCAL 102
#define ERR_READ 103
#define ERR_WRITE 104
#define ERR_RENAME 105

int update_file(const char* dst, const char* src) {
    FILE* dst_file;
    FILE* src_file;
    char temppath[MAX_PATH_LEN];
    if ( 0 > snprintf(temppath, MAX_PATH_LEN, "%s.tmp", dst)) {
        return ERR_MAX_LEN;
    }

    if (!(src_file = fopen(src, "r"))) {
        return ERR_REMOTE;
    }
    if (!(dst_file = fopen(temppath, "w"))) {
        fclose(src_file);
        return ERR_LOCAL;
    }
    setbuf(dst_file, NULL);
    char buf[BUFFER_LEN];
    for (;;) {
        int nread;
        int nwrite;
        nread = fread(buf, 1, BUFFER_LEN, src_file);
        if (0 == nread) {
            fclose(dst_file);
            if (feof(src_file)) {
                fclose(src_file);
                if (-1 == rename(temppath, dst)) {
                    remove(temppath);
                    return ERR_RENAME;
                } else {
                    return SUCC_UPDATE;
                }
            } else {
                fclose(src_file);
                remove(temppath);
                return ERR_READ;
            }
        } else {
            nwrite = fwrite(buf, 1, nread, dst_file);
            if (nwrite != nread) {
                fclose(dst_file);
                fclose(src_file);
                remove(dst);
                return ERR_WRITE;
            }
        }
    }
}

int make_dir(const char* lbase, char* filename) {
    char fullpath[MAX_PATH_LEN];
    int last = snprintf(fullpath, MAX_PATH_LEN, "%s%s", lbase, filename);
    while (fullpath[last] != '/') {
        last -= 1;
    }
    int first = strlen(lbase);
    for (int i = first; i <= last; i++) {
        if (fullpath[i] == '/') {
            fullpath[i] = '\0';
            if (mkdir(fullpath, S_IRWXU)) {
                if (errno != EEXIST) {
                    return -1;
                }
            }
            fullpath[i] = '/';
        }
    }
    return 0;
}

int process(char* file_name, const char* lbase, const char* rbase) {
    char lpath[MAX_PATH_LEN];
    char rpath[MAX_PATH_LEN];
    struct stat lstat;
    struct stat rstat;
    if ( 0 > snprintf(lpath, MAX_PATH_LEN, "%s%s", lbase, file_name)) {
        return ERR_MAX_LEN;
    }
    if ( 0 > snprintf(rpath, MAX_PATH_LEN, "%s%s", rbase, file_name)) {
        return ERR_MAX_LEN;
    }

    if (-1 == stat(rpath, &rstat)) {
        return ERR_REMOTE;
    }
    if (-1 == stat(lpath, &lstat)) {
        if (errno == ENOENT) {
            if (-1 == make_dir(lbase, file_name)) {
                return ERR_LOCAL;
            }
            return update_file(lpath, rpath);
        } else {
            return ERR_LOCAL;
        }
    }

    if (rstat.st_mtime <= lstat.st_mtime) {
        return SUCC_NO_UPDATE;
    } else {
        return update_file(lpath, rpath);
    }
}


int stdin_getline(char* buf, int max_len) {
    for (int i = 0; i < max_len; i++) {
        int c = getchar();
        if (c == EOF) {
            return EOF;
        } else if (c == '\n') {
    	    buf[i] = '\0';
    	    return i;
    	}
        buf[i] = (unsigned char)c;
    }
    exit(ERR_MAX_LEN);
}

int main(int argc, char *argv[]) {
    char file_name[MAX_PATH_LEN];
    if (argc != 3) {
        return -1;
    }
    const char * local_path = argv[1];
    const char * remote_path = argv[2];
    setbuf(stdout, NULL);
    while(EOF != stdin_getline(file_name, MAX_PATH_LEN)) {
        putchar(process(file_name, local_path, remote_path));
    }
    return 0;
}

