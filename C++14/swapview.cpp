#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <tuple>
#include <algorithm>
#include <vector>

#include <cstring>
#include <cmath>
#include <cctype>

#include <sys/types.h>
#include <dirent.h>
#include <error.h>

using namespace std;

// #define TARGET "Size:"     // test with Size: when swap is empty
#define TARGET "Swap:"
#define TARGETLEN 5

/////////////////////////////////////////////////////////////////////////////
// Helpers for Python to C++14 Conversion

template<typename... UArgs>
class StrFormatHelper{
    ostringstream sout;
    template<typename T>
    void format(T value){ sout<<value; }

    template<typename T, typename... TArgs>
    void format(T value, TArgs... args){
        sout<<value;
        format(args...);
    }

public:
    StrFormatHelper(UArgs... args){ format(args...); }
    operator string (){ return sout.str(); }
};

template<typename... UArgs>
string strformat(UArgs... args){
    return StrFormatHelper<UArgs...>(args...);
}

string readall(string path){
    ifstream fs(path);
    string buf((istreambuf_iterator<char>(fs)), istreambuf_iterator<char>());
    return buf;
}

vector<string> readlines(string path){
    ifstream fs(path);
    vector<string> result;
    for(string buf; getline(fs, buf);){
        result.push_back(buf);
    }
    return result;
}

int str2i(string str){
    int result;
    istringstream ssin(str);
    ssin>>result;
    return result;
}

vector<string> lsdir(string path){
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

typedef tuple<int, double, string> swap_info;


/////////////////////////////////////////////////////////////////////////////
// Converted C++14 code

string filesize(auto size){
    char units [] = "KMGT";
    auto left = fabs(size);
    int unit = -1;
    while( left > 1100 && unit < 3 ){
        left /= 1024;
        unit++;
    }
    if(unit == -1){
        return strformat(static_cast<int>(size), 'B');
    }else{
        if(size<0){
            left = -left;
        }
        return strformat(fixed, setprecision(1), left, units[unit], "iB");
    }
}

swap_info getSwapFor(int pid){
    string comm = readall(strformat("/proc/", pid, "/cmdline"));
    if(comm.length() > 0){
      replace(comm.begin(), comm.end(), '\0' , ' ');
      comm.pop_back();
    }
    double s=0.0;
    for(auto l: readlines(strformat("/proc/", pid, "/smaps"))){
        if(l.substr(0, TARGETLEN)==TARGET){
            s+=str2i(l.substr(TARGETLEN));
        }
    }
    return make_tuple(pid, s*1024.0, comm);
}

vector<swap_info> getSwap(){
    vector<swap_info> ret;
    for(string spid: lsdir("/proc")){
        int pid = str2i(spid);
        if(pid > 0) {
            auto item = getSwapFor(pid);
            if(get<1>(item) > 0){
                ret.push_back(item);
            }
        }
    }
    sort(ret.begin(), ret.end(),
        [](auto i, auto j){return get<1>(i) < get<1>(j);});
    return ret;
}

void format_print(auto pid, auto swap, auto command){
    cout<<setw(5)<<pid<<' '<<setw(9)<<swap<<' '<<command<<endl;
}

void format_print(swap_info swap){
    format_print(get<0>(swap), filesize(get<1>(swap)), get<2>(swap));
}

int main(int argc, char * argv[]){
    double t=0.0;
    auto result = getSwap();
    format_print("PID", "SWAP", "COMMAND");
    for(auto item: result){
        format_print(item);
        t+=get<1>(item);
    }
    cout<<"Total:"<<setw(9)<<filesize(t)<<endl;
    return 0;
}
