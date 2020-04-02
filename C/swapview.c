#define _POSIX_C_SOURCE 200809L
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dirent.h>
#include <errno.h>
#include <sys/types.h>

#define FORMAT "%7d %9s %s\n"
#define FORMAT_H "%7s %9s %s\n"
#define BUFSIZE 512
//#define TARGET "Size:" // For Test
#define TARGET "Swap:"
#define TARGETLEN (sizeof(TARGET) - 1)

#ifdef __GLIBC__
#include <error.h>
#define assure(exp)                                                            \
  if (!(exp))                                                                  \
  error(1, errno, "\"%s\" failed in %d", #exp, __LINE__)
#else
#define assure(exp)                                                            \
  if (!(exp)) {                                                                \
    fprintf(stderr, "\"%s\" failed in %d (%s)", #exp, __LINE__,                \
            strerror(errno));                                                  \
    exit(1);                                                                   \
  }
#endif

char *filesize(double size) {
  char units[] = "KMGT";
  double left = fabs(size);
  int unit = -1;

  char *buf;
  assure(buf = malloc(BUFSIZE));

  while (left > 1100 && unit < 3) {
    left /= 1024;
    unit++;
  }
  if (unit == -1) {
    assure(snprintf(buf, BUFSIZE, "%dB", (int)size) > 0);
  } else {
    if (size < 0)
      left = -left;
    assure(snprintf(buf, BUFSIZE, "%.1f%ciB", left, units[unit]) > 0);
  }
  return buf;
}

typedef struct {
  int pid;
  double size;
  char *comm;
} swap_info;

swap_info *getSwapFor(int pid) {
  char filename[BUFSIZE];
  FILE *fd = 0;
  size_t size = BUFSIZE;
  char *comm = malloc(size + 1); // +1 for last \0
  ssize_t len = 0;
  double s = 0.0;

  assure(snprintf(filename, BUFSIZE, "/proc/%d/cmdline", pid) > 0);
  if (!(fd = fopen(filename, "r")))
    goto err;
  for (int got; (got = fread(comm + len, 1, size - len, fd)) > 0; len += got) {
    assure(comm = realloc(comm, (size <<= 1) + 1)); // +1 for last \0
  }
  fclose(fd);

  for (char *p = comm; p < comm + len - 1; ++p)
    *p || (*p = ' '); // comm[len-1] is \0 or non-space
  comm[len] = '\0';   // assure string is terminated

  assure(snprintf(filename, BUFSIZE, "/proc/%d/smaps", pid) > 0);
  if (!(fd = fopen(filename, "r")))
    goto err;
  char *line;
  for (line = 0, size = 0;
       (len = getline(&line, &size, fd)) >= 0;
       free(line), line = 0, size = 0) {
    if (strncmp(line, TARGET, TARGETLEN) == 0)
      s += atoi(line + TARGETLEN);
  }
  free(line); // need to free when getline fail, see getline(3)
err:
  if (fd)
    fclose(fd);
  swap_info *ret;
  assure(ret = malloc(sizeof(swap_info)));
  ret->pid = pid;
  ret->size = s * 1024;
  ret->comm = comm;
  return ret;
}

int comp(const void *va, const void *vb) {
  double a = (**(swap_info **)va).size;
  double b = (**(swap_info **)vb).size;
  return (a > b) - (a < b);
}

swap_info **getSwap() {
  int size = 16;
  int length = 0;

  DIR *dp;
  struct dirent *dirp;
  assure(dp = opendir("/proc"));

  swap_info **ret;
  assure(ret = malloc(sizeof(swap_info *) * size));
  while ((dirp = readdir(dp)) != NULL) {
    char *end;
    int pid = (int)strtol(dirp->d_name, &end, 10);
    if (*end == '\0') {
      swap_info *swapfor = getSwapFor(pid);
      if (swapfor->size > 0) {
        if (length == size)
          assure(ret = realloc(ret, sizeof(swap_info *) * (size <<= 1)));
        ret[length++] = swapfor;
      } else {
        free(swapfor->comm);
        free(swapfor);
      }
    }
  }
  closedir(dp);

  qsort(ret, length, sizeof(swap_info *), comp);

  if (length == size)
    assure(ret = realloc(ret, sizeof(swap_info *) * (++size)));
  ret[length] = 0; // mark for end
  return ret;
}

int main() {
  swap_info **infos = getSwap(), **p = infos;
  double total = 0;
  printf(FORMAT_H, "PID", "SWAP", "COMMAND");
  for (; *p; ++p) {
    char *size = filesize((*p)->size);
    printf(FORMAT, (*p)->pid, size, (*p)->comm);
    total += (*p)->size;
    free(size);
    free((*p)->comm);
    free(*p);
  }
  free(infos);
  char *stotal = filesize(total);
  printf("Total: %8s\n", stotal);
  free(stotal);
  return 0;
}
