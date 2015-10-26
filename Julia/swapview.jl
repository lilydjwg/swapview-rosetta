#!/usr/bin/julia -f

function filesizeKB(size::Int)
    left = float(size)
    unit = 1
    while left > 1100 && unit < 4
        left /= 1024
        unit += 1
    end
    @inbounds u = ("KMGT")[unit]
    @sprintf("%.1f%siB", left, u)::ASCIIString
end

function getSwapFor(pid::ASCIIString)
    path = "/proc/$pid/smaps"
    fd = ccall(:open, Cint, (Ptr{Cchar}, Cint), path, Base.FS.JL_O_RDONLY)
    fd >= 0 || return 0
    stream = fdio(path, fd, true)
    str = ASCIIString(readbytes(stream))
    close(stream)
    s = 0
    @inbounds for _m in eachmatch(r"Size: *([0-9]*)", str)
        m = _m::RegexMatch
        s += parse(Int, m.captures[1]::SubString{UTF8String})
    end
    s
end

function getCmd(pid::ASCIIString)
    path = "/proc/$pid/cmdline"
    comm = open(readbytes, path)::Vector{UInt8}
    len = length(comm)
    @inbounds @simd for i in 1:len
        c = comm[i]
        comm[i] = ifelse(c == 0, UInt8(' '), c)
    end
    len >= 0 && comm[len] == ' ' && deleteat!(comm, len)
    convert(UTF8String, comm)
end

immutable SwapEntry
    pid::ASCIIString
    size::Int
    cmd::UTF8String
end

@inline Base.isless(e1::SwapEntry, e2::SwapEntry) = isless(e1.size, e2.size)

function getSwap()
    ret = SwapEntry[]
    @inbounds for _f in readdir("/proc")
        f = _f::ASCIIString
        isdigit(f) && begin
            s = getSwapFor(f)
            s > 0 && Collections.heappush!(ret, SwapEntry(f, s, getCmd(f)))
        end
    end
    return ret
end

function main()
    results = getSwap()
    @printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND")
    totalsize = 0
    @inbounds while !isempty(results)
        entry = Collections.heappop!(results)
        totalsize += entry.size
        @printf("%5s %9s %s\n", entry.pid, filesizeKB(entry.size), entry.cmd)
    end
    @printf("Total: %8s\n", totalsize == 0 ? "0B" : filesizeKB(totalsize))
end

main()
