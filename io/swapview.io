#!/usr/bin/env io

Number asFileSize := method(
	list("B", "KiB", "MiB", "GiB", "TiB") foreach(i, v,
		if((res := self / 1024 ** i) abs <= 1100 or i >= 4,
			return block(size, unit,
				if(unit == "B",
					size asString,
					size asString(0, 1)
				) asMutable appendSeq(unit)
			) call(res, v)
		)
	)
)

Directory asSwapUsed := method(
	retVal := list(name, 0, "")
	try(
		retVal atPut(1,
			fileNamed("smaps") readLines do(
				selectInPlace(beginsWithSeq("Swap:"))
				mapInPlace(splitNoEmpties at(1) asNumber)
			) reduce(+, 0)
		) atPut(2,
			fileNamed("cmdline") contents removeSuffix("\0") replaceSeq("\0", " ")
		)
	)
	return retVal
)

getSwap := method(
	getSwap = Directory with("/proc") directories do(
		selectInPlace(name asNumber isNan not)
		mapInPlace(asSwapUsed)
		selectInPlace(at(1) > 0)
		sortInPlaceBy(block(a, b, a at(1) < b at(1)))
	)
)

main := method(
	DynLib call("printf", "%5s %9s %s\n", "PID", "SWAP", "COMMAND")
	getSwap foreach(
		do(DynLib call("printf", "%5s %9s %s\n", at(0), at(1) asFileSize, at(2)))
	)
	total := getSwap mapInPlace(at(1)) reduce(+, 0)
	DynLib call("printf", "Total: %8s\n", total asFileSize)
)

main
