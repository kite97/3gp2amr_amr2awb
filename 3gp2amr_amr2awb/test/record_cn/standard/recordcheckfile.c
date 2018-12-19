#define _GNU_SOURCE
#include <dirent.h>     /* Defines DT_* constants */
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <string.h>
#include <sys/types.h>
#define handle_error(msg) \
        do { perror(msg); exit(EXIT_FAILURE); } while (0)

struct linux_dirent {
    long           d_ino;
    off_t          d_off;
    unsigned short d_reclen;
    char           d_name[];
};

#define BUF_SIZE (1024*1024*4)
int ulaw (const char* name)
{
    int slen = strlen(name);
    if (slen <= 5) return 0;
    if(name[slen-1] != 'w') return 0;
    if(name[slen-2] != 'a') return 0;
    if(name[slen-3] != 'l') return 0;
    if(name[slen-4] != 'u') return 0;
    if(name[slen-5] != '.') return 0;
    return 1;
}
int main(int argc, char *argv[])
{
    int fd, nread;
    char buf[BUF_SIZE];
    struct linux_dirent *d = NULL;
    int bpos;
    struct stat filestat;
    char d_type;
    int len,i;
    long success=0;
    long total=0;
    long array[1000]={0};
    fd = open(argc > 1 ? argv[1] : ".", O_RDONLY | O_DIRECTORY);
    if (fd == -1)
        handle_error("open");

    for ( ; ; ) {
        nread = syscall(SYS_getdents, fd, buf, BUF_SIZE);
        if (nread == -1)
            handle_error("getdents");

        if (nread == 0)
            break;
        for (bpos = 0; bpos < nread;) {
            d = (struct linux_dirent *) (buf + bpos);
	    //printf("%s\n", d->d_name);
	    //printf("name %s\n", d->d_name);
            d_type = *(buf + bpos + d->d_reclen - 1);
            if (d_type == DT_REG) {
            	//printf("name %s\n",d->d_name);
	        if (ulaw(d->d_name)) {
                    stat(d->d_name,&filestat);
                    len = filestat.st_size/160;
            	    //printf("name %s, len %d\n",d->d_name, len);
                    array[len]++;
                    total++;
                    if (len >= 495 && len <= 515) {
                        success++;  
                    }
	    	}
	    }
            bpos += d->d_reclen;
        }
    }
    //printf("total %ld\n",total);
    for(i = 0; i < 1000; i++) {
    	if (array[i] > 0) {
    	    printf("array[%4d] = %10ld  percent:%5.4f%%\n", i, array[i], (float)array[i]/(float)total*100);
    	}
    }
    printf("success = %8ld; total = %8ld;  percent= %5.4f%%;\n", success,total, (float)success/(float)total*100);
    exit(EXIT_SUCCESS);
}

