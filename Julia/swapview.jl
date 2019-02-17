#!/usr/bin/julia

using Printf

function filesizeKB(size::Int)
    left = float(size)
    unit = 1
    while left > 1100 && unit < 4
        left /= 1024
        unit += 1
    end
    @inbounds u = ("KMGT")[unit]
    @sprintf("%.1f%siB", left, u)::AbstractString
end

function getSwapFor(pid::AbstractString)
    s::Int = 0
    for m in eachmatch(r"Swap: *([0-9]*)", read("/proc/$pid/smaps", String))
        s += parse(Int, m.captures[1])
    end
    s
end

function getCmd(pid::AbstractString)
    comm::String = read("/proc/$pid/cmdline", String)
    !isempty(comm) && comm[end] == '\x00' && (comm = comm[1:end - 1])
    return replace(comm, "\x00" => " ")
end

function getSwap()
    ret = Any[]
    @inbounds for f in readdir("/proc")
        all(isdigit, f) && try
            s = getSwapFor(f)
            s > 0 && push!(ret, (f, s, getCmd(f)))
        catch
        end
    end
    sort!(ret, by=(x) -> x[2])
    return ret
end

function main()
    results = getSwap()
    @printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND")
    totalsize = 0
    @inbounds for (pid, swap, comm) in results
        @printf("%5s %9s %s\n", pid, filesizeKB(swap), comm)
    end
    @printf("Total: %8s\n", isempty(results) ? "0B" :
            filesizeKB(sum((x) -> x[2], results)))
end

main()
