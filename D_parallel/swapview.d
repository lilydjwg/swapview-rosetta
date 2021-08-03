#!/usr/bin/dmd -run

import std.stdio, std.file, std.path, std.string, std.conv, std.math, std.container, std.algorithm, std.parallelism, std.range;

string filesize(double size){
    string units = "KMGT";
    double left = cast(double)size.abs();
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
    auto ret = cast(ubyte[]) read(pid~"/cmdline");
    foreach(ref ubyte c; ret){
        if(c=='\0') c=' ';
    }
    if(ret[$-1] == ' ')
        ret.length--;
    return cast(string) ret;
}

ulong checkswap(string pid){
    ulong size = 0;
    File file = File(pid~"/smaps", "r");
    while (!file.eof()){
        string line = chomp(file.readln());
        if(!line.indexOf("Swap:")){
            size += to!ulong(line.split()[1]);
        }
    }
    return size * 1024 ;
}

struct SwapInfo
{
    string pid;
    ulong size;
    string comm;
};

SwapInfo swap_thread(string dir) {
    string pid = dir.baseName;
    if (pid.isNumeric) {
        try {
            ulong size = checkswap(dir);
            if (size)
                return SwapInfo(pid, size, getcmdln(dir));
        }catch(Exception) { }
    }
    return SwapInfo(null, 0, null);
}

auto getSwap(){
    string[] dir;
    foreach(string d; dirEntries("/proc", SpanMode.shallow))
        dir ~= [d];

    auto map = taskPool.amap!swap_thread(dir);
    return sort!"a.size < b.size"(map);
}


void main(){
    string m = "%7s %9s %s";
    double total = 0;
    auto result = getSwap();
    writeln(format(m, "PID", "SWAP", "COMMAND"));
    foreach(ref item; result){
        if (item.pid is null)
            continue;
        total += item.size;
        writeln(format(m, item.pid, filesize(item.size), item.comm));
    }
    writeln(format("Total: %10s", filesize(total)));
}

// vim: set et ts=4 sw=4
