#define _POSIX_C_SOURCE 200809L
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<ctype.h>
#include<errno.h>
#include<error.h>
#include<fcntl.h>
#include<unistd.h>
#include<sys/types.h>
#include<dirent.h>

#define FORMAT "%5d %9s %s\n"
#define BUFSIZE 512
//#define TARGET "Size:" // For Test
#define TARGET "Swap:"
#define TARGETLEN 5

#define assure(exp) if(!(exp)) error(1, errno, "\"%s\" failed in %d", #exp, __LINE__)

typedef struct {
  int pid;
  double size;
  char *comm;
} swap_info;

static swap_info **infos;
static size_t info_length;
static size_t info_size;

char *filesize(double size){
  char units[] = "KMGT";
  double left = fabs(size);
  int unit = -1;
  char *buf;
  assure(buf = malloc(BUFSIZE));

  while(left > 1100 && unit < 3){
    left /= 1024;
    unit++;
  }
  if(unit == -1){
    assure(snprintf(buf, BUFSIZE, "%dB", (int)size) > 0);
  }else{
    if(size < 0)
      left = -left;
    assure(snprintf(buf, BUFSIZE, "%.1f%ciB", left, units[unit]) > 0);
  }
  return buf;
}

swap_info *getSwapFor(int pid) {
  char filename[BUFSIZE];
  int fd;
  size_t size = BUFSIZE;
  char *comm = malloc(size + 1); // +1 for last '\0'
  ssize_t len=0;
  double sum = 0;
  int c = 0;
  char *p;
  swap_info *ret;

  assure(snprintf(filename, BUFSIZE, "/proc/%d/cmdline", pid) > 0);
  if ((fd = open(filename, O_RDONLY)) < 0)
    goto err;
  len = read(fd, comm, size);
  close(fd);

  for(p = comm; p < comm + len; ++p)
    *p || (*p = ' '); // comm[len-1] is '\0' or non-space
  comm[len]='\0'; // assure string is terminated

  assure(snprintf(filename, BUFSIZE, "/proc/%d/smaps", pid) > 0);
  if ((fd = open(filename, O_RDONLY)) < 0)
    goto err;
  dup2(fd, STDIN_FILENO); // redirect fd to stdin
  close(fd);
  while (c != EOF) {
    char buf[20];
    for (int i = 0; i < TARGETLEN; i++)
      buf[i] = getchar();
    if (!strncmp(buf, TARGET, TARGETLEN)) {
      int i = 0;
      while (isspace(c = getchar()));
      do {
        buf[i++] = c;
      } while (isdigit(c = getchar()));
      buf[i] = '\0';
      sum += atoi(buf);
    }
    while ((c = getchar()) != '\n' && c != EOF);
  }
  
err:
  assure(ret = malloc(sizeof(swap_info)));
  ret->pid = pid;
  ret->size = sum * 1024;
  ret->comm = comm;
  return ret;
}

int comp(const void *a, const void *b){
  double r = (*((swap_info **) a))->size - (*((swap_info **) b))->size;
  return (r > 0.0) - (r < 0.0);	// sign of double to int
}

void getSwap(){
  DIR *dp;
  struct dirent *dirp;
  assure(dp = opendir("/proc"));
  int stdin_fileno = dup(STDIN_FILENO);
  while((dirp = readdir(dp)) != NULL){
    int pid = atoi(dirp->d_name);
    if(pid > 0){
      swap_info *swapfor = getSwapFor(pid);
      if(swapfor->size > 0){
        if(info_length == info_size)
          assure(infos = realloc(infos, sizeof(swap_info *) * (info_size <<= 1)));
        infos[info_length++] = swapfor;
      }else{
        free(swapfor->comm);
        free(swapfor);
      }
    }
  }
  dup2(stdin_fileno, STDIN_FILENO); // restore stdin fileno
  close(stdin_fileno);
  closedir(dp);

  qsort(infos, info_length, sizeof(swap_info *), comp);
}

int main(int argc, char *argv[]){
  double total = 0;
  info_size = 16;
  assure(infos = malloc(sizeof(swap_info *) * info_size));

  getSwap();
  printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND");
  for(int i = 0; i < info_length; ++i) {
    char *size = filesize(infos[i]->size);
    printf(FORMAT, infos[i]->pid, size, infos[i]->comm);
    total += infos[i]->size;
    free(size);
    free(infos[i]->comm);
    free(infos[i]);
  }

  free(infos);
  char *stotal = filesize(total);
  printf("Total: %8s\n", stotal);
  free(stotal);
  return 0;
}
