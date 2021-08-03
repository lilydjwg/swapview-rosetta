import java.io.File
import scala.io.Source

class SwapView(val pid: Int = 0, val size: Double = 0, val comm: String = "") {
	def filesize(size: Double): String = {
		val units = "KMGT"
		var left = Math.abs(size)
		var unit = -1

		while (left > 1100 && unit < 3) {
			left /= 1024
			unit += 1
		}

		if (unit == -1)  f"${size.asInstanceOf[Int]}%dB"
		else if (size < 0) f"${-left}%.1f${units(unit)}%ciB" 
		else f"${left}%.1f${units(unit)}%ciB" 
	}

	def getSwapFor(pid: Int): SwapView = {
		try {
			val comm = Source.fromFile(f"/proc/${pid}%d/cmdline", "UTF-8").getLines.reduceLeft(_+_).replaceAll("\u0000" , " ").init
			var s = 0.0
			for (line <- Source.fromFile(f"/proc/${pid}%d/smaps", "UTF-8").getLines if line.startsWith("Swap:")) {
				var a = line split " "
				s += a(a.size - 2).toInt
			}
			new SwapView(pid, s*1024, comm)
		} catch {
			case e: Exception => new SwapView(pid, 0, "")
		}
	}

	def getSwap: List[SwapView] = {
		var ret = List[SwapView]()
		for (fpid <- (new File("/proc").listFiles)) {
			try {
				var pid = fpid.getName.toInt
				var s = getSwapFor(pid)
				if (s.size > 0)  ret = s :: ret
			} catch {
				case e: NumberFormatException => ()
			} // do nothing for parse error
		}
		ret.sortBy(_.size)
	}
}

object SwapView extends App {
	val swapview = new SwapView
	val results  = swapview.getSwap
	printf("%7s %9s %s\n", "PID", "SWAP", "COMMAND")
	var x = 0.0
	for (res <- results) {
		printf("%7d %9s %s\n", res.pid, swapview.filesize(res.size), res.comm)
		x += res.size
	}
	printf("Total: %10s\n", swapview.filesize(x))
}
