#!/usr/bin/julia -f

@inline function filesize(size::Int)
    left::Float64 = abs(size)
    unit::Int = 0
    while left > 1100 && unit < 4
        left /= 1024
        unit += 1
    end
    return if unit == 0
        @sprintf("%dB", size)
    else
        @inbounds u = ("KMGT")[unit]
        @sprintf("%.1f%siB", size < 0 ? -left : left, u)
    end
end

@inline function getSwapFor(pid::ASCIIString)
    comm::String = open(readall, "/proc/$pid/cmdline")
    if !isempty(comm) && comm[end] == '\x00'
        comm = comm[1:end - 1]
    end
    comm = replace(comm, "\x00", " ")
    local s::Int = 0
    open("/proc/$pid/smaps") do fh
        for line in eachline(fh)
            s += startswith(line, "Swap:") ? int(split(line)[2]) : 0
        end
    end
    return s * 1024, comm
end

@inline function getSwap()
    files = readdir("/proc")
    ret = (ASCIIString, Int, String)[]
    sizehint!(ret, length(files))
    for f::ASCIIString in files
        if isdigit(f)
            try
                s, comm = getSwapFor(f)
                if s > 0
                    push!(ret, (f, s, comm))
                end
            end
        end
    end
    sort!(ret, by=(x) -> x[2])
    return ret
end

function main()
    @printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND")
    results = getSwap()
    for (pid, swap, comm) in results
        @printf("%5s %9s %s\n", pid, filesize(swap), comm)
    end
    @printf("Total: %8s\n", filesize(isempty(results) ? 0 :
                                     sum((x) -> x[2], results)))
end

main()
