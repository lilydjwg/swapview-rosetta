#!/usr/bin/julia -f

@inline function filesizeKB(size::Int)
    left::Float64 = size
    unit::Int = 1
    while left > 1100 && unit < 4
        left /= 1024
        unit += 1
    end
    @inbounds u = ("KMGT")[unit]
    @sprintf("%.1f%siB", left, u)
end

@inline function getSwapFor(pid::ASCIIString)
    s::Int = 0
    for m in eachmatch(r"Size: *([0-9]*)", open(readall, "/proc/$pid/smaps"))
        s += int(m.captures[1])
    end
    return s
end

@inline function getCmd(pid::ASCIIString)
    comm::String = open(readall, "/proc/$pid/cmdline")
    !isempty(comm) && comm[end] == '\x00' && (comm = comm[1:end - 1])
    return replace(comm, "\x00", " ")
end

@inline function getSwap()
    ret = Any[]
    for f in readdir("/proc")
        isdigit(f) && try
            f::ASCIIString
            s = getSwapFor(f)
            s > 0 && push!(ret, (f, s, getCmd(f)))
        end
    end
    sort!(ret, by=(x) -> x[2])
    return ret
end

function main()
    @printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND")
    results = getSwap()
    for (pid, swap, comm) in results
        @printf("%5s %9s %s\n", pid, filesizeKB(swap), comm)
    end
    @printf("Total: %8s\n", isempty(results) ? "0B" :
            filesizeKB(sum((x) -> x[2], results)))
end

main()
