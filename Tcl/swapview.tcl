#!/usr/bin/env tclsh

proc fileSize {bytes {level 0}} {
	set units {B KiB MiB GiB}
	if {$bytes >= 1100 && $level < 3} {
		return [fileSize [expr $bytes/1024.0] [expr $level+1]]
	}
        if {$level == 0} {
        	return [format "%dB" $bytes]
        } else {
        	return [format "%.1lf%s" $bytes [lindex $units $level]]
        }
}

proc readFile {path} {
	set fd [open $path]
	set contents [read $fd]
	close $fd
	return $contents
}

proc getSwapInfo {pid} {
	# return {pid swap-use cmdline}
	if {![string is digit $pid]} {error error}
	set cmdline [string trim [string map {\0 " "} [readFile "/proc/$pid/cmdline"]]]
	set fd [open "/proc/$pid/smaps"]
	set totalswap 0
	while {1} {
		if {[eof $fd]} break
		if {[scan [gets $fd] "Swap: %lld" swapuse]} {
			incr totalswap $swapuse
		}
	}
	close $fd
	return [list $pid [expr $totalswap*1024] $cmdline]
}

set total 0
set all {}
foreach pid [lmap dir [glob /proc/*] {file tail $dir}] {
	if {![catch {getSwapInfo $pid} info] && [llength $info] == 3 && [lindex $info 1] != 0} {
		lappend all $info
		incr total [lindex $info 1]
	}
}
puts [format "%5s %9s %s" PID SWAP COMMAND]
foreach i [lsort -integer -index 1 $all] {
	puts [format "%5s %9s %s" [lindex $i 0] [fileSize [lindex $i 1]] [lindex $i 2]]
}
puts [format "Total: %8s" [fileSize $total]]

