#!/bin/bash

cd /proc

for pid in [0-9]*; do
    command=$(cat /proc/$pid/cmdline 2>/dev/null)
    [[ $? -ne 0 ]] && continue

    swap=$(
        awk '
            BEGIN  { total = 0 }
            /Swap/ { total += $2 }
            END    { print total }
        ' /proc/$pid/smaps 2>/dev/null
    )
    [[ $? -ne 0 ]] && continue

    if (( $swap > 0 )); then
        if [[ "${head}" != "yes" ]]; then
            echo -e "PID\tSWAP\tCOMMAND"
            head="yes"
        fi

        echo -e "${pid}\t${swap}\t${command}"
    fi
done
