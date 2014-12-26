#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <errno.h>
#include <error.h>
#include <sys/types.h>
#include <dirent.h>

#define SFORMAT "%5s %9s %s\n"
#define FORMAT "%5d %9s %s\n"
#define BUFSIZE 32
//#define TARGET "Size:" // For Test
#define TARGET "Swap:"

#define die(msg) error(1, errno, "Error %d: %s\n",__LINE__ , msg)

char* filesize(double size){
	char units [] = "KMGT";
	double left = fabs(size);
	int unit = -1;

	char *buf;
	if(!(buf= malloc(BUFSIZE))) die("malloc");

	while( left > 1100 && unit < 3 ){
		left /= 1024;
		unit++;
	}
	if(unit == -1){
		if(snprintf(buf, BUFSIZE, "%dB", (int)size) < 0) die("snprintf");
	}else{
		if(size<0){
			left = -left;
		}
		if(snprintf(buf, BUFSIZE, "%.1f%ciB", left, units[unit]) < 0) die("snprintf");
	}
	return buf;
}

typedef struct {int pid; double size; char *comm;} swap_info;

swap_info * getSwapFor(int pid){
	swap_info *ret ;
	if(!(ret=malloc(sizeof(swap_info)))) die("malloc");
	char filename [BUFSIZE];
	if(snprintf(filename, BUFSIZE, "/proc/%d/cmdline", pid) < 0) die("snprintf");

	FILE* fd=0;
	char *comm = 0; size_t size=0; int len;
	double s=0.0;

	if(!(fd = fopen(filename, "r"))) goto err;
	if((len=getline(&comm, &size, fd)) < 0) goto err;
	fclose(fd); fd=0;

	for(char *p=comm; p < comm+len-1; ++p){ // len not including terminal \0
		if(!*p) *p=' ';
	}

	size=0;
	if(snprintf(filename, BUFSIZE, "/proc/%d/smaps", pid) < 0) die("snprintf");
	if(!(fd = fopen(filename, "r"))) goto err;
	char *line;
	for(line = 0; (len=getline(&line, &size, fd)) > 0;){
		if(strncmp(line, TARGET, 5) == 0){
			s+=atoi(line+5);
		}
		free(line);
		line=0;
		size=0;
	}
	free(line);
err:
	if(fd) fclose(fd);
	ret->pid = pid;
	ret->size = s*1024;
	ret->comm = comm;
	return ret;
}


int comp(const void* a, const void* b){
	double r=(*((swap_info**)a))->size - (*((swap_info**)b))->size;
	return (r>0.0) - (r<0.0); // sign of double to int
}

swap_info ** getSwap(){
	swap_info **ret;
	int size=16;
	if(!(ret=malloc(sizeof(swap_info*)*size))) die("malloc");
	int length=0;

	DIR *dp;
	struct dirent *dirp;
	if((dp = opendir("/proc")) == NULL) die("opendir");

	while ((dirp = readdir(dp)) != NULL) {
		int pid = atoi(dirp->d_name);
		if(pid > 0){
			if(length==size){
				size<<=1;
				if(!(ret=realloc(ret, sizeof(swap_info*)*size))) die("realloc");
			}
			swap_info * swapfor = getSwapFor(pid);
			if(swapfor->size > 0){
				ret[length++] = swapfor;
			}else{
				free(swapfor->comm);
				free(swapfor);
			}
		}
	}
	closedir(dp);

	qsort(ret, length, sizeof(swap_info*), comp);

	if(length==size){
		size<<=1;
		if(!(ret=realloc(ret, sizeof(swap_info*)*size))) die("realloc");
	}
	ret[length]=0;
	return ret;
}

int main(int argc, char * argv[]){
	printf(SFORMAT, "PID", "SWAP", "COMMAND");
	
	swap_info** info=getSwap();
	double total=0;
	for(swap_info** p=info; *p; ++p){
		char* size=filesize((*p)->size);
		printf(FORMAT, (*p)->pid, size, (*p)->comm);
		total += (*p)->size;
		free(size);
		free((*p)->comm);
		free(*p);
	}
	free(info);
	char* stotal = filesize(total);
	printf("Total: %8s\n", stotal);
	free(stotal);
	return 0;
}