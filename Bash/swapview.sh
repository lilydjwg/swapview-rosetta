#!/bin/bash
# globalized consts
base=1024
unit=(K M G T) nunit=4 # assert nunit == ${#unit[@]}
filesize(){
    local pos powed # int64, also declare -i
    declare -i size="$1"
    if ((size < 1100)); then
        printf '%s\n' "${size}B"
        return
    fi
    # See also ``base ** pow`` (bash) and ``base ^ pow`` (bc).
    # Should this be precomputed?
    for ((pos=0, powed=1024; size / powed > 1100 && pos < nunit; pos++, powed *= 1024)); do :; done
    printf '%.1f%siB\n' "$(bc <<< "scale=1; $size / ($powed)")" "${unit[pos]}"
}

getSwap(){
    local sumfile=/tmp/sum.$$.$RANDOM
    cd /proc
	
    (declare -i sum=0; for pid in [0-9]*; do
        # FIXME: better if printf('%q ', read(/proc/$pid/cmdline).split(NUL))
        #        splitnul: https://stackoverflow.com/questions/8677546/
        command=$(tr '\0' ' ' 2>/dev/null </proc/$pid/cmdline) &&
        command=${command%' '} &&

        # assuming awk works faster than read.
        swap=$(
            awk '
                BEGIN   { total = 0 }
                /Swap:/ { total += $2 }
                END     { print total }
            ' "/proc/$pid/smaps" 2>/dev/null
        ) &&

        if (( swap > 0 )); then
            sum+=swap
            printf '%7s %9s %s\n' "$pid" "$(filesize $((swap*1024)))" "$command"
        fi || continue
    done; printf '%s\n' $((sum * 1024)) > $sumfile) | sort -k2 -h
    printf "Total: %10s\n" "$(filesize "$(<$sumfile)")"
    rm "$sumfile"
}

printf "%7s %9s %s\n" PID SWAP COMMAND
getSwap
