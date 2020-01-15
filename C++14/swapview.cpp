#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <tuple>
#include <algorithm>
#include <cmath>
#include <sys/types.h>
#include <dirent.h>
#include <error.h>
#include <vector>

using namespace std;

constexpr const char * TARGET = "Swap:";
constexpr const size_t TARGETLEN = 5;

using swap_info = tuple<int, double, string>;

string filesize(double size) {
    static const char * units = "KMGT";
    auto left = abs(size); 
    int unit = -1;
    while( left > 1100 && unit < 3 ){
        left /= 1024;
        unit++;
    }
    if(unit == -1) return (to_string((int)size) + 'B');

    ostringstream sout;
    sout << fixed << setprecision(1) << (size > 0 ? left : -left) << units[unit] << "iB";
    return sout.str();
}

bool starts_with (string const& s1, string const& s2) {
    if(s1.size() < s2.size()) return false;
    auto r = mismatch(s2.begin(), s2.end(), s1.begin());
    return (r.first == s2.end());
}

swap_info getSwapFor(int pid){
    ifstream cmd_inf("/proc/" + to_string(pid) + "/cmdline");
    string comm((istreambuf_iterator<char>(cmd_inf)), istreambuf_iterator<char>());
    if(!comm.empty()) {
        if(comm.back() == 0) comm.pop_back();
        replace(comm.begin(), comm.end(), '\0' , ' ');
    }
    double s=0.0;
    ifstream inf("/proc/" + to_string(pid) + "/smaps");
    string l;
    while(getline(inf, l)) {
        if(starts_with(l, TARGET)) {
            s += strtol(l.c_str() + TARGETLEN, nullptr, 10);
        }
    }
    return make_tuple(pid, s*1024.0, comm);
}

vector<swap_info> getSwap(){
    vector<swap_info> ret;
    DIR * dp = opendir("/proc");
    if(dp) {
        while(struct dirent * dirp = readdir(dp)) {
            if(int pid = strtol(dirp->d_name, nullptr, 10)) {
                auto item = getSwapFor(pid);
                if(get<1>(item) > 0)
                    ret.push_back(item);
            }
        }
        closedir(dp);
    }
    sort(ret.begin(), ret.end(), 
        [](auto const& i, auto const& j){return get<1>(i) < get<1>(j);});
    return ret;
}

int main(int argc, char * argv[]){
    std::ios::sync_with_stdio(false);
    double t=0.0;
    cout << setw(7) << "PID" << ' ' << setw(9) << "SWAP" << ' ' << "COMMAND" << "\n";
    for(auto const& item: getSwap()){
        cout << setw(7) << get<0>(item) 
             << ' ' << setw(9) << filesize(get<1>(item))
             << ' ' << get<2>(item)
             << "\n";
        t+=get<1>(item);
    }
    cout<<"Total:"<<setw(9)<<filesize(t)<<"\n";
    return 0;
}
