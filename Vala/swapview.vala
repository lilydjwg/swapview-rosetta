string filesize(double size){
    string units = "KMGT";
    double left = Math.fabs(size);
    int unit = -1;
    while(left > 1100 && unit < 3){
        left /= 1024;
        unit++;
    }
    if(unit == -1){
        return "%dB".printf((int)size);
    }else{
        if(size < 0) left = -left;
        return "%.1f%ciB".printf(left, units[unit]);
    }
}

struct SwapView{
    int64 pid;
    double size;
    string comm;
}

SwapView getSwapFor(int64 pid){
    try{
        DataInputStream dis = new DataInputStream(
            File.new_for_path(@"/proc/$(pid)/cmdline").read());
        size_t len;
        string? comm = dis.read_upto("", 0,out len);
        if(comm!=null)
            for(int i=0; i<len-1; ++i)
                if(comm.data[i]==0)
                    comm.data[i]=' ';
        double s = 0;
        dis = new DataInputStream(
            File.new_for_path(@"/proc/$(pid)/smaps").read());
        string? l;
        while((l=dis.read_line())!=null)
            if(l.has_prefix("Swap:")){
                string[] a = l.split(" ");
                s += int.parse(a[a.length-2]);
            }
        return {pid, s*1024, comm};
    }catch(Error e){
        return {pid, 0, ""};
    }
}

List<SwapView?> getSwap(){
    List<SwapView?> ret = new List<SwapView?>();
    try{
        FileEnumerator enumerator = File.new_for_path("/proc/")
            .enumerate_children("standard::*", FileQueryInfoFlags.NONE);
        FileInfo fpid;
        while((fpid = enumerator.next_file()) != null){
            int64 pid = 0;
            if(fpid.get_file_type () == FileType.DIRECTORY &&
                int64.try_parse(fpid.get_name(), out pid)){
                SwapView s = getSwapFor(pid);
                if(s.size > 0) 
                    ret.append(s);
            }
        }
    }catch(Error e){} // do nothing for errors
    ret.sort((a, b) => (a.size < b.size) ? -1 : (a.size > b.size) ? 1 : 0);
    return ret;
}

static int main (string[] args){
    List<SwapView?> results = getSwap();
    stdout.printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND");
    double t = 0.0;
    foreach(SwapView s in results){
        stdout.printf("%5lld %9s %s\n", s.pid, filesize(s.size), s.comm);
        t += s.size;
    }
    stdout.printf("Total: %8s\n", filesize(t));
    return 0;
}
