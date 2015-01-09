#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <vector>

#include <sys/types.h>
#include <dirent.h>
#include <error.h>
#include <errno.h>

using namespace std;

// #define TARGET "Size:"     // test with Size: when swap is empty
#define TARGETLEN 5
#define TARGET "Swap:"
#define ABS(x) ((x<0)?(-x):(x))

/////////////////////////////////////////////////////////////////////////////

int inline str2i(const string & s){
    int ret = 0;
    for(string::const_iterator i = s.begin() ;i < s.end(); ++i){
        if ( *i <= '9' && *i >= '0')
            ret = ret * 10 + ( *i - '0');
    }
    return ret;
}

void inline lsdir(const string & path, vector<string> & ret){
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
    double left = ABS(size);
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
        if(buf.substr(0, TARGETLEN)==TARGET){
            s += str2i(buf);
        }
    }
    swap_info ret = {pid, comm, s*1024.0};
    return ret;
}

void getSwap(vector<swap_info> & ret){
    vector<string> dir;
    lsdir("/proc", dir);
    for(vector<string>::iterator itr=dir.begin(); itr<dir.end(); ++itr){
        int pid = str2i(*itr);
        if(pid > 0) {
            swap_info item = getSwapFor(pid, *itr);
            if(item.size > 0)
            {
                ret.push_back(item);
            }
        }
    }
    sort(ret.begin(), ret.end());
}

template<typename T1, typename T2, typename T3>
void format_print(const T1 & pid, const T2 & swap, const T3 & command){
    cout<<setw(5)<<pid<<' '<<setw(9)<<swap<<' '<<command<<'\n';
}

void format_print(const swap_info& swap){
    format_print(swap.pid, filesize(swap.size), swap.comm);
}

int main(int argc, char * argv[]){
    double t=0.0;
    vector<swap_info> result;
    getSwap(result);
    format_print("PID", "SWAP", "COMMAND");
    for(vector<swap_info>::iterator itr = result.begin(); itr < result.end(); ++itr){
        format_print(*itr);
        t += itr->size;
    }
    cout<<"Total:"<<setw(9)<<filesize(t)<<endl;
    return 0;
}
