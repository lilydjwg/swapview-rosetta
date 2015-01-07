#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <vector>

#include <cmath>
#include <cctype>

#include <sys/types.h>
#include <dirent.h>
#include <error.h>
#include <errno.h>

#include <omp.h>
using namespace std;

// #define TARGET "Size:"     // test with Size: when swap is empty
#define TARGETLEN 5
#define TARGET "Swap:"

/////////////////////////////////////////////////////////////////////////////
void readlines(string path, vector<string> & ret){
    ifstream fs(path.c_str());
    for(string buf; getline(fs, buf);){
        ret.push_back(buf);
    }
}

int str2i(string str){
    int result;
    istringstream ssin(str);
    ssin>>result;
    return result;
}

void lsdir(string path, vector<string> & ret){
    DIR *dp;
    struct dirent *dirp;
    if((dp  = opendir(path.c_str())) == NULL) {
        error(errno, errno, "opening %s \n", path.c_str());
    }

    while ((dirp = readdir(dp)) != NULL) {
        ret.push_back(string(dirp->d_name));
    }
    closedir(dp);
}

struct swap_info{
    int pid; string comm; double size;
    int operator< (swap_info& other){
        return size < other.size;
    }
};


/////////////////////////////////////////////////////////////////////////////

string filesize(double size){
    char units [] = "KMGT";
    double left = fabs(size);
    int unit = -1;
    while( left > 1100 && unit < 3 ){
        left /= 1024;
        unit++;
    }
    ostringstream sout;
    if(unit == -1){
        sout << static_cast<int>(size) << 'B';
    }else{
        if(size<0){
            left = -left;
        }
        sout << fixed << setprecision(1) << left << units[unit] << "iB";
    }
    return sout.str();
}

swap_info getSwapFor(int pid){
    ostringstream cmdline, smaps;
    cmdline << "/proc/" << pid << "/cmdline";
    ifstream fs(cmdline.str().c_str());
    string comm((istreambuf_iterator<char>(fs)), istreambuf_iterator<char>());
    if(comm.length() > 0){
      replace(comm.begin(), comm.end(), '\0' , ' ');
      comm.erase(comm.end()-1);
    }
    double s=0.0;
    smaps << "/proc/" << pid << "/smaps";
    vector<string> lines;
    readlines(smaps.str(), lines);
    for(vector<string>::iterator itr=lines.begin(); itr<lines.end(); ++itr ){
        if(itr->substr(0, TARGETLEN)==TARGET){
            s+=str2i(itr->substr(TARGETLEN));
        }
    }
    swap_info ret = {pid, comm, s*1024.0};
    return ret;
}


void getSwap(vector<swap_info> & ret){
    vector<string> dir;
    lsdir("/proc", dir);
#pragma omp parallel for
    for(vector<string>::iterator itr=dir.begin(); itr<dir.end(); ++itr){
        int pid = str2i(*itr);
        if(pid > 0) {
            swap_info item=getSwapFor(pid);
            if(item.size > 0)
#pragma omp critical
            {
                ret.push_back(item);
            }
        }
    }
    sort(ret.begin(), ret.end());
}

template<typename T1, typename T2, typename T3>
void format_print(T1 pid, T2 swap, T3 command){
    cout<<setw(5)<<pid<<' '<<setw(9)<<swap<<' '<<command<<endl;
}

void format_print(swap_info& swap){
    format_print(swap.pid, filesize(swap.size), swap.comm);
}

int main(int argc, char * argv[]){
    omp_set_num_threads(omp_get_num_procs()*4);
    double t=0.0;
    vector<swap_info> result;
    getSwap(result);
    format_print("PID", "SWAP", "COMMAND");
    for(vector<swap_info>::iterator itr= result.begin(); itr<result.end();++itr){
        format_print(*itr);
        t+=itr->size;
    }
    cout<<"Total:"<<setw(9)<<filesize(t)<<endl;
    return 0;
}
