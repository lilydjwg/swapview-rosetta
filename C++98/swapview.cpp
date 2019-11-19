#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <vector>

#include <cmath>
#include <cassert>

#include <sys/types.h>
#include <dirent.h>
#include <error.h>
#include <errno.h>

#ifdef USE_OMP
#include <omp.h>
#endif
using namespace std;

// #define TARGET "Size:"     // test with Size: when swap is empty
#define TARGETLEN 5
#define TARGET "Swap:"

/////////////////////////////////////////////////////////////////////////////

vector<string> inline lsdir(const string & path){
    vector<string> ret;
    DIR *dp;
    struct dirent *dirp;
    if((dp  = opendir(path.c_str())) == NULL) {
        error(errno, errno, "opening %s \n", path.c_str());
    }
    while ((dirp = readdir(dp)) != NULL) {
        ret.push_back(string(dirp->d_name));
    }
    closedir(dp);
    return ret;
}

struct swap_info{
    int pid; string comm; double size;
    int operator< (const swap_info& other){
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

swap_info getSwapFor(int pid, const string & spid){
    ifstream fs((string("/proc/") + spid + string("/cmdline")).c_str());
    string comm((istreambuf_iterator<char>(fs)), istreambuf_iterator<char>());
    if(comm.length() > 0){
        replace(comm.begin(), comm.end(), '\0' , ' ');
        if(*(comm.end()-1) == ' ')
            comm.erase(comm.end()-1);
    }

    double s=0.0;
    ifstream sfs((string("/proc/") + spid + string("/smaps")).c_str());
    for(string buf; getline(sfs, buf);){
        if(buf.size() > TARGETLEN &&
            mismatch(TARGET, TARGET + TARGETLEN, buf.begin())
            .first == TARGET + TARGETLEN){
            s += atoi(buf.c_str()+TARGETLEN);
        }
    }
    swap_info ret = {pid, comm, s*1024.0};
    return ret;
}

struct not_digit { bool operator() (char x){ return !isdigit(x); } };

vector<swap_info> getSwap(){
    vector<swap_info> ret;
    vector<string> dir = lsdir("/proc");
#ifdef USE_OMP
#pragma omp parallel for
#endif
    for(vector<string>::iterator itr=dir.begin(); itr<dir.end(); ++itr){
        if(find_if(itr->begin(), itr->end(), not_digit()) == itr->end()){
            int pid = atoi(itr->c_str());
            assert(pid > 0);
            swap_info item = getSwapFor(pid, *itr);
            if(item.size > 0)
#ifdef USE_OMP
#pragma omp critical
#endif
            {
                ret.push_back(item);
            }
        }
    }
    sort(ret.begin(), ret.end());
    return ret;
}

template<typename T1, typename T2, typename T3>
void format_print(const T1 & pid, const T2 & swap, const T3 & command){
    cout<<setw(5)<<pid<<' '<<setw(9)<<swap<<' '<<command<<'\n';
}

void format_print(const swap_info& swap){
    format_print(swap.pid, filesize(swap.size), swap.comm);
}

int main(int argc, char * argv[]){
#ifdef USE_OMP
    omp_set_num_threads(omp_get_num_procs()*4);
#endif
    std::ios::sync_with_stdio(false);
    double t=0.0;
    vector<swap_info> result = getSwap();
    format_print("PID", "SWAP", "COMMAND");
    for(vector<swap_info>::iterator itr = result.begin(); itr < result.end(); ++itr){
        format_print(*itr);
        t += itr->size;
    }
    cout<<"Total:"<<setw(9)<<filesize(t)<<"\n";
    return 0;
}
