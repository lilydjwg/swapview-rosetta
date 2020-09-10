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

string read_to_end(File file) {
    static ubyte[65536] buffer;
    ubyte[] ret;
    while (!file.eof()) {
        ret ~= file.rawRead(buffer[]);
    }
    return cast(string)ret;
}

ulong checkswap(string pid){
    import std.array : join, split;
    import std.algorithm : startsWith, findSplitBefore;
    import std.string : stripLeft;
    ulong size = 0;
    File file = File(pid~"/smaps", "r");
    auto data = file.read_to_end;
    foreach(line; data.split("\n")) {
        if(line.startsWith("Swap:")){
            line = line[5..$].stripLeft;
            size += to!int(cast(string)line.findSplitBefore(" ")[0]);
        }
    }
    return size * 1024;
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

    auto map = taskPool.amap!swap_thread(dir, 1);
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
    writeln(format("Total: %8s", filesize(total)));
}

// vim: set et ts=4 sw=4
