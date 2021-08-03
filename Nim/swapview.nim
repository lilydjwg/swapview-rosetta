import os, strutils, pegs, algorithm

type
    swap_info = tuple[pid: int, swap: float, cmd: string]

const TARGET = "Swap:"
const units = "KMGT"

proc filesize(size: float): string =
    var unit = -1
    var left = abs(size)

    while left > 1100 and unit < 3:
        left /= 1024
        unit += 1
    if unit == -1: return $int(size) & "B"

    if size < 0:
        left = -left
    return formatFloat(left, ffDecimal, 1) & units[unit] & "iB"

proc get_swap_for(pid: int): swap_info =
    var cmdfs: File
    var str: string = ""

    result = (pid, 0.0, "")
    if not open(cmdfs, "/proc/" & $pid & "/cmdline"):
        return
    if readLine(cmdfs, result.cmd):
        var i = 0
        while i < result.cmd.len:
            if result.cmd[i] == '\0': result.cmd[i] = ' '
            inc(i)
        delete(result.cmd, i, i)
    close(cmdfs)

    var smapsfs: File
    if open(smapsfs, "/proc/" & $pid & "/smaps"):
        while readLine(smapsfs, str):
            # it'll be too slow with pegs
            if startsWith(str, TARGET):
              var r = @[""]
              if match(str, peg"@' '+ {[0-9]+}", r):
                result.swap += parseFloat(r[0])
        close(smapsfs)
    result.swap *= 1024.0

proc get_swap(): seq[swap_info] =
    result = @[]
    for s in walkDirs("/proc/*"):
      var tail = splitPath(s).tail
      if allCharsInSet(tail, Digits):
        var pid = parseInt(tail)
        var si = get_swap_for(pid)
        if si.swap > 0:
          result.add(si)

    result.sort(proc (x, y: swap_info): int = cmp(x.swap, y.swap))

proc `$` (x: swap_info): string =
    var pidstr = $x.pid
    var swapstr = filesize(x.swap)
    spaces(max(0, 7 - pidstr.len)) & pidstr & " " &
        spaces(max(0, 9 - swapstr.len)) & swapstr & " " & x.cmd

# main
echo "    PID      SWAP COMMAND"
var infos = get_swap()
var t: float = 0
for i in infos:
    echo i
    t += i.swap
var total = filesize(t)
echo "Total: " & spaces(max(0, 10 - total.len)) & total
