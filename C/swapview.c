#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#define require(exp) do { \
    if (!(exp)) { \
        fprintf(stderr, "\"%s\" failed in %s:%d (%s)\n", #exp, __FILE__,  __LINE__, strerror(errno)); \
        exit(EXIT_FAILURE); \
    } \
} while (false)

#define PROC_PATH "/proc"
#define SMAPS_FIELD "Swap:"
#define PID_LEN 6
#define BUFSZ 128

typedef char pidstr_t[PID_LEN];

bool is_pid(const char* fname) {
    while (*fname) {
        if (!isdigit(*fname)) return false;
        fname++;
    }
    return true;
}

bool gen_pids(pidstr_t* ppid) {
    static DIR* proc_dir = NULL;
    static bool init = true;
    // init generator
    if (init) {
        proc_dir = opendir(PROC_PATH);
        require(proc_dir != NULL);
        init = false;
    }
    // generate next pid
    struct dirent* ent;
    do {
        errno = 0;
        ent = readdir(proc_dir);
        if (!ent) {
            require(errno == 0);
            return false;
        }
    } while (!is_pid(ent->d_name));
    strncpy(*ppid, ent->d_name, PID_LEN - 1);
    (*ppid)[PID_LEN - 1] = '\0';
    return true;
}

typedef struct swap_info swap_info_t;
struct swap_info {
    pidstr_t pid;
    char* cmdline;
    size_t swap_size;
};

char* read_cmdline(const char* fname) {
    char* buf = (char*) malloc(BUFSZ);
    size_t cap = BUFSZ;
    FILE* f = fopen(fname, "r");
    if (!f) {
        *buf = '\0';
        return buf;
    }
    size_t read = 0;
    int c;
    while (c = getc(f), c != EOF) {
        if (c == '\0') c = ' ';
        if (read == cap) {
            buf = realloc(buf, cap *= 2);
            require(buf != NULL);
        }
        buf[read++] = c;
    }
    buf[read] = '\0';
    return buf;
}

bool match(FILE* f, const char* str) {
    size_t matched = 0;
    size_t len = strlen(str);
    while (matched != len) {
        int c = fgetc(f);
        if (c == EOF) return false;
        if (c == str[matched]) matched++;
        else matched = 0;
    }
    return true;
}

size_t read_swap_size(const char* fname) {
    FILE* f = fopen(fname, "r");
    if (!f) return 0;
    size_t swap_size = 0;
    while (match(f, SMAPS_FIELD)) {
        size_t i;
        require(fscanf(f, "%zu", &i) == 1);
        swap_size += i * 1024;
    }
    return swap_size;
}

swap_info_t get_swap_info(const pidstr_t pid) {
    swap_info_t retval;
    memcpy(retval.pid, pid, PID_LEN);
    char path[sizeof(PROC_PATH) + PID_LEN + 10];
    sprintf(path, PROC_PATH "/%s/", pid);
    char* cwd = path + strlen(path);
    strcpy(cwd, "cmdline");
    retval.cmdline = read_cmdline(path);
    strcpy(cwd, "smaps");
    retval.swap_size = read_swap_size(path);
    return retval;
}

typedef struct sibuffer sibuffer_t;
struct sibuffer {
    size_t cap;
    size_t len;
    swap_info_t data[];
};

#define INIT_SIZE 4
#define EXPAND_FACTOR 2

sibuffer_t* new_sibuffer() {
    sibuffer_t* buf = (sibuffer_t*) malloc(sizeof(sibuffer_t) + sizeof(swap_info_t) * INIT_SIZE);
    require(buf != NULL);
    buf->cap = INIT_SIZE;
    buf->len = 0;
    return buf;
}

void append(sibuffer_t** pbuf, swap_info_t si) {
    if ((*pbuf)->len == (*pbuf)->cap) {
        sibuffer_t* newbuf = (sibuffer_t*) realloc(*pbuf,
                sizeof(sibuffer_t)
                + sizeof(swap_info_t) * (*pbuf)->cap * EXPAND_FACTOR);
        require(newbuf != NULL);
        *pbuf = newbuf;
        (*pbuf)->cap *= EXPAND_FACTOR;
    }
    (*pbuf)->data[(*pbuf)->len++] = si;
}

void del_sibuffer(sibuffer_t* buf) {
    for (size_t i = 0; i < buf->len; i++) {
        free(buf->data[i].cmdline);
    }
    free(buf);
}

int si_less(const void* x, const void* y) {
    return ((swap_info_t*)x)->swap_size
         - ((swap_info_t*)y)->swap_size;
}

sibuffer_t* get_all_swap_infos() {
    sibuffer_t* buf = new_sibuffer();
    pidstr_t pid;
    while (gen_pids(&pid)) {
        swap_info_t si = get_swap_info(pid);
        if (si.swap_size > 0) append(&buf, si);
    }
    qsort(buf->data, buf->len, sizeof(swap_info_t), si_less);
    return buf;
}

void print_file_size(double size) {
    int unit = 0;
    char units[] = " KMGT";
    while (size > 1024 && unit < 5) {
        size /= 1024;
        unit++;
    }
    if (!unit) printf("%lgB", size);
    else printf("%lg%cB", size, units[unit]);
}

int main() {
    printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND");
    double total = 0;
    sibuffer_t* sis = get_all_swap_infos();
    for (size_t i = 0; i < sis->len; i++) {
        printf("%5s %9zu %s\n", sis->data[i].pid, sis->data[i].swap_size, sis->data[i].cmdline);
        total += sis->data[i].swap_size;
    }
    del_sibuffer(sis);
    printf("Total: ");
    print_file_size(total);
    putchar('\n');
    return EXIT_SUCCESS;
}
