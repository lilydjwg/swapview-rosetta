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
#define COMM_LEN 17
#define PID_LEN 6

typedef char pidstr_t[PID_LEN];

bool is_pid(const char* fname) {
    while (*fname) {
        if (!isdigit(*fname)) return false;
        fname++;
    }
    return true;
}

const pidstr_t* gen_pids() {
    static pidstr_t pid;
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
            return NULL;
        }
    } while (!is_pid(ent->d_name));
    strncpy(pid, ent->d_name, PID_LEN - 1);
    pid[PID_LEN - 1] = '\0';
    return &pid;
}

typedef struct swap_info swap_info_t;
struct swap_info {
    pidstr_t pid;
    char comm[COMM_LEN];
    int swap_size;
};

void read_comm(FILE* f, char comm[COMM_LEN]) {
    size_t read = 0;
    int c;
    while (c = fgetc(f), c != EOF && c != '\n' && read < COMM_LEN - 1) {
        comm[read++] = c;
    }
    comm[read] = '\0';
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

int read_swap_size(FILE* f) {
    int swap_size = 0;
    while (match(f, "Swap:")) {
        int i;
        require(fscanf(f, "%d", &i) == 1);
        swap_size += i;
    }
    return swap_size;
}

swap_info_t get_swap_info(const pidstr_t pid) {
    swap_info_t retval = { "", "", 0 };
    memcpy(retval.pid, pid, PID_LEN);
    char path[sizeof(PROC_PATH) + PID_LEN + 10];
    sprintf(path, PROC_PATH "/%s/", pid);
    char* cwd = path + strlen(path);
    strcpy(cwd, "comm");
    FILE* f;
    if ((f = fopen(path, "r"))) {
        read_comm(f, retval.comm);
        fclose(f);
    }
    strcpy(cwd, "smaps");
    if ((f = fopen(path, "r"))) {
        retval.swap_size = read_swap_size(f);
    }
    return retval;
}

typedef struct sibuffer* sibuffer_t;
struct sibuffer {
    swap_info_t* data;
    size_t cap;
    size_t len;
};

#define INIT_SIZE 4
#define EXPAND_FACTOR 2

sibuffer_t new_sibuffer() {
    sibuffer_t buf = (sibuffer_t) malloc(sizeof(struct sibuffer));
    require(buf != NULL);
    buf->data = (swap_info_t*) malloc(sizeof(swap_info_t) * INIT_SIZE);
    buf->cap = INIT_SIZE;
    buf->len = 0;
    return buf;
}

void append(sibuffer_t buf, swap_info_t si) {
    if (buf->len == buf->cap) {
        swap_info_t* newdata = (swap_info_t*) realloc(buf->data, sizeof(swap_info_t) * buf->cap * EXPAND_FACTOR);
        require(newdata != NULL);
        buf->data = newdata;
        buf->cap *= EXPAND_FACTOR;
    }
    buf->data[buf->len++] = si;
}

int si_less(const void* x, const void* y) {
    return ((swap_info_t*)x)->swap_size
         - ((swap_info_t*)y)->swap_size;
}

void get_all_swap_infos(swap_info_t** pparr, size_t* count) {
    sibuffer_t buf = new_sibuffer();
    const pidstr_t* ppid;
    while ((ppid = gen_pids())) {
        swap_info_t si = get_swap_info(*ppid);
        if (si.swap_size > 0) append(buf, si);
    }
    qsort(buf->data, buf->len, sizeof(swap_info_t), si_less);
    *pparr = buf->data;
    *count = buf->len;
    free(buf);
}

void print_file_size(int size) {
    int unit = 0;
    char units[] = " KMGT";
    while (size > 1024 && unit < 5) {
        size /= 1024;
        unit++;
    }
    if (!unit) printf("%dB", size);
    else printf("%d%cB", size, units[unit]);
}

int main() {
    printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND");
    int total = 0;
    swap_info_t* sis;
    size_t count;
    get_all_swap_infos(&sis, &count);
    for (size_t i = 0; i < count; i++) {
        printf("%5s %9d %s\n", sis[i].pid, sis[i].swap_size, sis[i].comm);
        total += sis[i].swap_size;
    }
    free(sis);
    printf("Total: ");
    print_file_size(total);
    putchar('\n');
    return EXIT_SUCCESS;
}
