#!/usr/bin/dmd -run

import std.stdio, std.file, std.path, std.string, std.conv, std.math;

string filesize(int size){
    string units = "KMGT";
    int left = size.abs();
    int unit = -1;

    while(left > 1100 && unit < 3){
        left /=1024;
        unit += 1;
    }

    if(unit == -1){
        return format("%sB", size);
    }else{
        if(size < 0)
            left = -left;
        return format("%s%siB", left, units[unit]);
    }
}

string getcmdln(string pid){
    File file = File("/proc/"~pid~"/cmdline", "r");
    return file.readln();
}

int checkswap(string pid){
    int size = 0;
    File file = File("/proc/"~pid~"/smaps", "r");
    while (!file.eof()){
        string line = chomp(file.readln());
        if(!line.indexOf("Swap:")){
            size += to!int(line.split()[1]);
        }
    }
    return size * 2014 ;
}

void main(){
    string m = "%5s %9s %s";
    writeln(format(m , "PID", "SWAP", "COMMAND"));
    foreach(DirEntry dirs; dirEntries("/proc", SpanMode.shallow)){
        string pid = baseName(dirs.name);
        if(pid.isNumeric()){
            int size = checkswap(pid);
            if(size)
                writeln(format(m, pid, filesize(size), getcmdln(pid)));
        }
    }
}
