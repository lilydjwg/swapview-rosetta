#!/usr/bin/dmd -run

import std.stdio, std.file, std.path, std.string, std.conv, std.math, std.container, std.algorithm;

string filesize(double size){
    string units = "KMGT";
    double left = size.fabs();
    int unit = -1;

    while(left > 1100 && unit < 3){
        left /=1024;
        unit += 1;
    }

    if(unit == -1){
        return format("%dB", to!int(size));
    }else{
        if(size < 0)
            left = -left;
        return format("%.1f%siB", left, units[unit]);
    }
}

string getcmdln(string pid){
    auto ret = cast(ubyte[]) read("/proc/"~pid~"/cmdline");
    foreach(ref ubyte c; ret){
        if(c=='\0') c=' ';
    }
    if(ret[$-1] == ' ')
        ret = ret[0 .. $-1];
    ret ~= '\0';
    return cast(string) ret;
}

double checkswap(string pid){
    double size = 0;
    File file = File("/proc/"~pid~"/smaps", "r");
    while (!file.eof()){
        string line = chomp(file.readln());
        if(!line.indexOf("Swap:")){
            size += to!int(line.split()[1]);
        }
    }
    return size * 1024 ;
}

struct SwapInfo
{
    int pid;
    double size;
    string comm;

    int opCmp(ref const SwapInfo s) const {
        double r = size - s.size;
        return (r > 0) - (r < 0);
    }
};

SwapInfo[] getSwap(){
    SwapInfo[] ret;
    foreach(DirEntry dirs; dirEntries("/proc", SpanMode.shallow)){
        string pid = baseName(dirs.name);
        if(pid.isNumeric()){
            try{
                double size = checkswap(pid);
                if(size)
                    ret ~= SwapInfo(to!int(pid), size, getcmdln(pid));
            }catch(Exception){} // do nothing for error
        }
    }
    sort(ret);
    return ret;
}


void main(){
    string m = "%7s %9s %s";
    double total=0;
    auto result=getSwap();
    writeln(format(m , "PID", "SWAP", "COMMAND"));
    foreach(SwapInfo item; result){
        total += item.size;
        writeln(format(m , item.pid, filesize(item.size), item.comm));
    }
    writeln(format("Total: %10s", filesize(total)));
}
