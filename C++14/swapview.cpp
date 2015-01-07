#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <tuple>
#include <algorithm>

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
    return string((istreambuf_iterator<char>(fs)), istreambuf_iterator<char>());
}

template<typename T>
struct range_wrapper {
    struct iterator {
        iterator(range_wrapper * rw) : rw(rw) {
            if(rw) ++(*this); // read the first file
        }

        typename T::value_type
        operator * (void) const { 
            return rw->t.get();
        }

        iterator& operator ++ (void) {
            rw->t.next(); 
            return *this;
        }

        bool end(void) const { return (!rw) || (rw->t.end()); }

        bool operator == (iterator const& i) const {
            return (end() == i.end());
        }

        bool operator != (iterator const& i ) const {
            return (end() != i.end());
        }
        range_wrapper * rw;
    };

    template<typename... Args>
    range_wrapper(Args... args)
        : t(args...)
    { }

    iterator begin() { return iterator(this); }
    iterator end() { return iterator(nullptr); }

    T t;
};

struct readlines {
    readlines (string const& path) : fs(path) { } 
    string const& get(void) const { return cur_line; }
    void next(void) { getline(fs, cur_line); }
    bool end(void) const { return !fs; }

    using value_type = string const&;
    ifstream fs;
    string cur_line;
};

struct lsdir {
    lsdir(string const& path) {
        if((dp = opendir(path.c_str())) == nullptr) {
            error(errno, errno, "opening %s \n", path.c_str());
        }
    }
    ~lsdir() { if(dp) closedir(dp); }

    const char * get(void) const { return dirp->d_name; }
    void next(void) { dirp = readdir(dp); }
    bool end(void) const { return dirp == nullptr; }

    using value_type = const char *;
    DIR * dp;
    struct dirent * dirp = nullptr;
};

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

bool starts_with (string const& s1, string const& s2) {
    if(s1.size() < s2.size())
        return false;

    auto r = mismatch(s2.begin(), s2.end(), s1.begin());
    return (r.first == s2.end());
}

swap_info getSwapFor(int pid){
    string comm = readall(strformat("/proc/", pid, "/cmdline"));
    if(comm.length() > 0){
      replace(comm.begin(), comm.end(), '\0' , ' ');
      comm.pop_back();
    }
    double s=0.0;
    for(auto const& l: range_wrapper<readlines>(strformat("/proc/", pid, "/smaps"))){
        if(starts_with(l, TARGET)) {
            s += strtol(l.c_str() + TARGETLEN, nullptr, 10);
        }
    }
    return make_tuple(pid, s*1024.0, comm);
}

vector<swap_info> getSwap(){
    vector<swap_info> ret;
    for(auto const& spid: range_wrapper<lsdir>("/proc")){
        int pid = strtol(spid, nullptr, 10);
        if(pid > 0) {
            auto item = getSwapFor(pid);
            if(get<1>(item) > 0){
                ret.push_back(item);
            }
        }
    }
    sort(ret.begin(), ret.end(),
        [](auto const& i, auto const& j){return get<1>(i) < get<1>(j);});
    return ret;
}

void format_print(auto const& pid, auto const& swap, auto const& command){
    cout<<setw(5)<<pid<<' '<<setw(9)<<swap<<' '<<command<<endl;
}

void format_print(swap_info const& swap){
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
