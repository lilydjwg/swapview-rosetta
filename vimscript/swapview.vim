"Exception handling was ignored
"Sorting was not done, since entries were printed as it was processed
"The output **could** be printed(put) in current buffer
fun! SwapView()
	let loc = '/proc'
	let pids= glob(loc.'/[0-9]*',0,1)
	echo "PID\tSWAP\tCOMMAND"
	let total = 0
	for pidf in pids
		let cmds = readfile(pidf.'/cmdline')
		if empty(cmds)
			continue
		endif
		let cmd = split(cmds[0],'\n')[0]
		if empty(cmd)
			continue
		endif
		let entries = filter(map(filter(readfile(pidf.'/smaps'),'v:val =~ "^Swap:"'), 'split(v:val)[-2]'),'v:val>0')
		if empty(entries)
			continue
		endif
		exe 'let thisSwap = ' . join(entries, '+')
		let total += thisSwap
		echo matchstr(pidf, '[^/]*$')."\t".thisSwap."\t".cmd
	endfor
	echo "Total: ".total
endf
" vim:tw=78:ts=2
