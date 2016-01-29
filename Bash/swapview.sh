#!/bin/bash

# globalized consts
base=1024
unit=(K M G T) nunit=4 # assert nunit == ${#unit[@]}
filesize(){
    [[ $1 -eq $1 ]] || return    # msk-num-check
    local size="$1" pos powed # uint64, also declare -i
    if ((size < 1100)); then
        printf '%s\n' "${size}B"
        return
    fi
    # See also ``base ** pow`` (bash) and ``base ^ pow`` (bc).
    for ((pos=0, powed=1; size / powed > 1100 && pos < nunit; pos++; powed *= 1024)); do :; done
    printf '%.2g%siB\n' "$(bc <<< "$size / ($powed)")" "${unit[pos]}"
}

getSwap(){
    declare -i sumsize
    local sumfile=/tmp/sum.$$.$RAMDOM
    cd /proc
    (declare -i sum=0; for pid in [0-9]*; do
        # FIXME: better if printf('%q ', read(/proc/$pid/cmdline).split(NUL))
        #        splitnul: https://stackoverflow.com/questions/8677546/
        command=$(tr '\0' ' ' 2>/dev/null </proc/$pid/cmdline) &&
        command=${command%' '} &&
        
        # assuming awk works faster than read.
        swap=$(
            awk '
                BEGIN  { total = 0 }
                /Swap/ { total += $2 }
                END    { print total }
            ' /proc/$pid/smaps 2>/dev/null
        ) &&

        if (( swap > 0 )); then
            sum+=swap
            printf "%5s %9s %s\n" "$pid" "$(filesize $((swap*1024)))" "$command"
        fi || continue
    done; printf '%s\n' $((sum * 1024)) > $sumfile) | sort -k2 -h
    printf "Total: %8s\n" "$(filesize $(<$sumfile))"
    rm $sumfile
}

printf "%5s %9s %s\n" PID SWAP COMMAND
getSwap
