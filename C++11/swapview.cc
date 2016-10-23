#define _POSIX_C_SOURCE 200809L
#include <sys/types.h>
#include <dirent.h>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <iterator>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <cmath>


//#define TARGET "Size" // for test
#define TARGET "Swap"

using namespace std;


vector<string> read_dir(const string & path){
    unique_ptr<DIR, decltype(closedir)*> dp(opendir(path.c_str()), closedir);
    vector<string> rv;
    while (auto dirp = readdir(dp.get()))
        rv.push_back(dirp->d_name);
    return rv;
}

struct SwapInfo {
    int pid;
    long long int size;
    string comm;
};

string file_size(long long int size) {
    static const char units [] = "KMGT";
    double left = fabs(size);
    int unit = -1;
    while (left > 1100 && unit < 3) {
        left /= 1024;
        unit++;
    }
    ostringstream out;
    if (unit == -1) {
        out << size << 'B';
    } else {
        if (size < 0)
            left = -left;
        out << fixed << setprecision(1) << left << units[unit] << "iB";
    }
    return out.str();
}

bool starts_with(const string & a, const string & b) {
    return a.compare(0, b.length(), b) == 0;
}

SwapInfo get_swap_info(const string & pid) {
    auto rv = SwapInfo{stoi(pid), 0};
    if (rv.pid == 0)
        return rv;

    string cmdline;
    try {
        ifstream cmds(string("/proc/") + pid + "/cmdline");
        cmdline = string(istreambuf_iterator<char>(cmds), {});
    } catch (const ios_base::failure &) {}
    if (cmdline.length() == 0)
        return rv;
    if (cmdline[cmdline.length()-1] == '\0')
        cmdline.pop_back();
    replace(cmdline.begin(), cmdline.end(), '\0', ' ');
    rv.comm = move(cmdline);

    ifstream inf(string("/proc/") + pid + "/smaps");
    string l;
    while (getline(inf, l))
        if (starts_with(l, TARGET))
            rv.size += strtoll(l.c_str()+sizeof(TARGET), nullptr, 10);

    rv.size *= 1024;
    return rv;
}

int main() {
    vector<SwapInfo> all_swap_info;
    for (auto const & n : read_dir("/proc/")) {
        if (!all_of(n.begin(), n.end(), ::isdigit))  // "::" make gcc happy
            continue;
        auto tmp = get_swap_info(n);
        if (tmp.size > 0)
            all_swap_info.push_back(move(tmp));
    }
    sort(all_swap_info.begin(), all_swap_info.end(),
            [](SwapInfo const & a, SwapInfo const & b) { return a.size < b.size; });

    cout << setw(5) << "PID" << ' ' << setw(9) << "SWAP" << ' ' << "COMMAND" << endl;
    long long int total = 0;
    for (auto const & x : all_swap_info) {
        cout << setw(5) << x.pid << ' ' << setw(9) << file_size(x.size) << 
            ' ' << x.comm << endl;
        total += x.size;
    }
    cout << "Total: " << setw(8) << file_size(total) << endl;
    return 0;
}
