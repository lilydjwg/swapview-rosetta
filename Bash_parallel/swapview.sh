#!/bin/bash

function filesize(){
    size=$1
    if [[ $1 -le 1100 ]] ; then
        echo "${size}B"
        return
    fi
    arr=($(bc -l <<EOF
left=$size;
unit=-1;
while(left>1100 && unit<3){
    left /= 1024
    unit += 1
}
left
unit
EOF
))
    left=${arr[0]}
    unit=${arr[1]}
    units="KMGT"
    printf "%.1f%siB\n" "$left" "${units:$unit:1}"
}

function getSwapFor(){
    pid=$1
    command=$(tr '\0' ' ' 2>/dev/null </proc/$pid/cmdline)
    [[ $? -ne 0 ]] && return
    len=$((${#command}-1))
    if [[ "${command:$len:1}"x = " "x ]]; then
        command="${command:0:$len}"
    fi

    swap=$(
        awk '
            BEGIN  { total = 0 }
            /Swap/ { total += $2 }
            END    { print total }
        ' /proc/$pid/smaps 2>/dev/null
    )
    [[ $? -ne 0 ]] && return

    if (( swap > 0 )); then
        fs=$(filesize $((swap*1024)))
        echo $swap >>$sumfile
        printf "%5s %9s %s\n" "$pid" "$fs" "$command"
    fi
}

function getSwap(){
    sumfile=/tmp/sum$RAMDOM
    > $sumfile
    export -f getSwapFor
    export -f filesize
    export sumfile
    cd /proc
    ls -d [0-9]* | parallel --no-notice -I% "getSwapFor %" | sort -k2 -h
    sumsize=$(paste -sd+ <$sumfile)
    if [[ "$sumsize"x = ""x ]]; then
        sumsize=0
    fi
    total=$(filesize $(( $(echo $sumsize | bc) *1024)))
    printf "Total: %8s\n" $total
    rm $sumfile
}

printf "%5s %9s %s\n" PID SWAP COMMAND
getSwap
