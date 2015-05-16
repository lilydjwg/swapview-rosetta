#/usr/bin/tclsh

proc fileSize {bytes {level 0}} {
	set units {B K M G}
	if {$bytes >= 1024 && $level < 3} {
		return [fileSize [expr $bytes/1024] [expr $level+1]]
	}
	return "$bytes[lindex $units $level]"
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
	set cmdline [readFile "/proc/$pid/cmdline"]
	foreach l [split [readFile "/proc/$pid/smaps"] "\n"] {
		if {[scan $l "Swap: %d" swapuse]} break
	}
	return [list $pid [expr $swapuse*1024] $cmdline]
}

set total 0
puts [format "%5s %9s %s" PID SWAP COMMAND]
foreach pid [lmap dir [glob /proc/*] {file tail $dir}] {
	if {![catch {getSwapInfo $pid} info] && [llength $info] == 3 && [lindex $info 1] != 0} {
		puts [format "%5d %9s %s" [lindex $info 0] [fileSize [lindex $info 1]] [lindex $info 2]]
		
	}
}
puts [format "Total: %8s" [fileSize $total]]
