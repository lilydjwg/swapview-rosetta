#define _POSIX_C_SOURCE 200809L
#include <sys/types.h>
#include <dirent.h>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <sstream>
#include <cstdio>
#include <memory>
#include <stdio.h>
#include <iomanip>


//#define TARGET "Size" // for test
#define TARGET "Swap"


std::vector<std::string> read_dir(std::string path){
    std::unique_ptr<DIR, int (*)(DIR*)> dp(opendir(path.c_str()), closedir);
    struct dirent *dirp = nullptr;
    auto rv = std::vector<std::string>{};
    while((dirp = readdir(dp.get())) != nullptr){
        rv.push_back(dirp->d_name);
    }
    return rv;
}

std::string read_file(std::string const & fn){
    try{
        std::ifstream in(fn);
        return std::string(std::istreambuf_iterator<char>(in),
                           std::istreambuf_iterator<char>());
    }
    catch(const std::ios_base::failure &){
        return "";
    }
}

struct SwapInfo{
    int pid {};
    long long int size{};
    std::string comm;
};
std::string output_size(long long int size){
    std::ostringstream out;
    static const char units [] = "KMGT";
    double left = fabs(size);
    int unit = -1;
    while( left > 1100 && unit < 3 ){
        left /= 1024;
        unit++;
    }
    if(unit == -1){
       out << static_cast<int>(size) << 'B';
    }else{
        if(size<0){
            left = -left;
        }
        out << std::fixed << std::setprecision(1) << left << units[unit] << "iB";
    }
    return out.str();
}

std::ostream & operator<<(std::ostream & out, SwapInfo const & si){
    auto size = output_size(si.size);
    out << std::setw(5) << si.pid;
    out << " " << std::setw(9) << size;
    out << " " << si.comm;
    return out;
}

SwapInfo get_swap_info(std::string const & pid){
    auto rv = SwapInfo{};
    rv.pid = std::atoi(pid.c_str());
    if(rv.pid == 0)
        return rv;
    auto cmdline = read_file(std::string("/proc/") + pid + "/cmdline");
    if(cmdline.length() == 0)
        return rv;
    if(cmdline[cmdline.length() - 1] == '\0')
        cmdline.pop_back();
    std::replace(cmdline.begin(), cmdline.end(), '\0', ' ');
    rv.comm = std::move(cmdline);
    std::unique_ptr<FILE,int (*)(FILE *)> fp(fopen((std::string("/proc/") + pid + "/smaps").c_str(), "r"), fclose);
    if(fp.get() == nullptr)
        return rv;
    char * line_tmp = nullptr;
    size_t size = 0;
    auto deleter = [](char ** ptr){std::free(*ptr);};
    std::unique_ptr<char *, decltype(deleter)> line(&line_tmp, deleter);
    while(true){
        auto len = getline(&line_tmp, &size, fp.get());
        if(len <= 0)
            break;
        long long int size;
        if(sscanf(line_tmp, TARGET ": %lld", &size)){
            rv.size += size * 1024;
        }
    }
    return rv;
}

int main(){
    std::vector<SwapInfo> all_swap_info;
    for(auto const & n : read_dir("/proc/")){
        if(n[0] == '.')
            continue;
        auto tmp = get_swap_info(n);
        if(tmp.size > 0){
            all_swap_info.push_back(std::move(tmp));
        }
    }
    auto sorter = [](SwapInfo const & a,SwapInfo const &b){
        return a.size < b.size;
    };
    std::sort(all_swap_info.begin(), all_swap_info.end(), sorter);
    long long int total = 0;
    printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND");
    for(auto const & x : all_swap_info){
        std::cout << x << "\n";
        total += x.size;
    }
    auto size = output_size(total);
    std::printf("Total: %8s\n", size.c_str());
    return 0;
}
