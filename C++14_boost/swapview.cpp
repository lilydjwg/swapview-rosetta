#include <iostream>
#include <string>
#include <fstream>
#include <tuple>
#include <algorithm>
#include <cmath>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>

using namespace std;

constexpr const char * TARGET = "Swap:";
constexpr size_t TARGETLEN = 5;

using swap_info = tuple<int, double, string>;

auto filesize(double size) {
    constexpr const char * units = "KMGT";
    auto left = abs(size); 
    int unit = -1;
    while( left > 1100 && unit < 3 ){
        left /= 1024;
        unit++;
    }
    return (unit == -1) ? boost::format("%dB") % size
        : boost::format("%|.1f|%||iB") % (size > 0 ? left : -left) % units[unit];
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
    while(getline(inf, l))
        if(boost::starts_with(l, TARGET)) 
            s += strtol(l.c_str() + TARGETLEN, nullptr, 10);
    return make_tuple(pid, s*1024.0, comm);
}

vector<swap_info> getSwap() {
    vector<swap_info> ret;
    for(auto const& entry : boost::make_iterator_range(boost::filesystem::directory_iterator("/proc"), boost::filesystem::directory_iterator())) 
        if(int pid = strtol(entry.path().filename().c_str(), nullptr, 10)) {
            auto item = getSwapFor(pid);
            if(get<1>(item) > 0)
                ret.push_back(item);
        }
    sort(ret.begin(), ret.end(), [](auto const& i, auto const& j){return get<1>(i) < get<1>(j);});
    return ret;
}

int main(int argc, char * argv[]){
    std::ios::sync_with_stdio(false);
    boost::format format("%|5| %|9| %||");
    cout << format % "PID" % "SWAP" % "COMMAND" << "\n";
    double t=0.0;
    for(auto const& item: getSwap()){
        cout << format % get<0>(item) % filesize(get<1>(item)) % get<2>(item) << "\n";
        t+=get<1>(item);
    }
    cout<< boost::format("Total:%|9|") % filesize(t)<<"\n";
    return 0;
}
