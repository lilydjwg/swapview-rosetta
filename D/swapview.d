#!/usr/bin/dmd -run

import std.stdio, std.file, std.path, std.conv, std.math, std.container, std.algorithm;

string filesize(double size){
    import std.format : format;
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
    if(ret[$-1] == '\0')
        ret.length--;
    foreach(ref ubyte c; ret){
        if(c=='\0') c=' ';
    }
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

double checkswap(string pid){
    import std.array : join, split;
    import std.algorithm : startsWith, findSplitBefore;
    import std.string : stripLeft;
    double size = 0;
    File file = File("/proc/"~pid~"/smaps", "r");
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
    int pid;
    double size;
    string comm;

    int opCmp(ref const SwapInfo s) const {
        double r = size - s.size;
        return (r > 0) - (r < 0);
    }
};

SwapInfo[] getSwap(){
    import std.string : isNumeric;
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


void main() {
    import std.format : format;
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
