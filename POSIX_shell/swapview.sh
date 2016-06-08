#!/bin/sh -

readonly CURRENT_USER_ID="$(id -u)"
readonly PRINTF_FORMAT='%5s %9s %s\n'

main() {
    printf "$PRINTF_FORMAT" 'PID' 'SWAP' 'COMMAND'
    find '/proc' -maxdepth 2 -regextype posix-basic -regex '^/proc/[[:digit:]]\+$' -type d -print | \
        while read dir_path || [ -n "$dir_path" ]
        do
            # Dash doesn't support here-string, so I use here-document instead.
            local pid="$(grep -o '[[:digit:]]\+$' <<EOF
$dir_path
EOF
            )"
            local smaps_file="${dir_path}/smaps"
            local cmdline_file="${dir_path}/cmdline"
            local swap_size=0
            local cmd=''
            if [ -f "$smaps_file" ] && \
                [ -r "$smaps_file" ] && \
                ( [ "$CURRENT_USER_ID" -eq 0 ] || [ -O "$smaps_file" ] )
            then
                swap_size="$(grep -F 'Swap:' "$smaps_file" | \
                            awk 'BEGIN{ sum = 0 }
                                 { sum += $2 }
                                 END{ print sum }')"
            fi
            if [ "$(printf '%s\n' "${swap_size} == 0" | bc)" -eq 1 ]
            then
                continue
            fi
            if [ -f "$cmdline_file" ] && \
                [ -r "$cmdline_file" ] && \
                ( [ "$CURRENT_USER_ID" -eq 0 ] || [ -O "$cmdline_file" ] )
            then
                cmd="$(tr '\0' ' ' < "$cmdline_file")"
            fi
            printf "$PRINTF_FORMAT" "$pid" "$swap_size" "$cmd"
        done | sort -k2n | \
        awk -v "printf_format=${PRINTF_FORMAT}" \
            'function convert_file_size_from_kB(size, _ARGV_END_, unit, units) {
                split("KMGT", units, "")
                unit=1
                while (size > 1100 && unit < 4) {
                    size /= 1024
                    unit += 1
                }
                return sprintf("%.1f%siB", size, units[unit])
            }
            function join_field(start, end, sep, _ARGV_END_, i, str) {
                str=$start
                for (i = start+1; i <= end; i++) {
                    str = str sep $i
                }
                return str
            }
            BEGIN{
                total=0
            }
            {
                total += $2
                printf(printf_format, $1, convert_file_size_from_kB($2), join_field(3, NF, " "))
            }
            END{
                printf("Total: %s\n", convert_file_size_from_kB(total))
            }'
}

main "$@"
