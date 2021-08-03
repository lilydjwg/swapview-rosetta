import java.util.*;
import java.math.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.io.*;

public class SwapView{
	public int pid;
	public double size;
	public String comm;
	public SwapView(int p, double s, String c){ pid = p; size = s; comm = c; }

	public static String filesize(double size){
		String units = "KMGT";
		double left = Math.abs(size);
		int unit = -1;
		while(left > 1100 && unit < 3){
			left /= 1024;
			unit++;
		}
		if(unit == -1){
			return String.format("%dB", (int)size);
		}else{
			if(size < 0) left = -left;
			return String.format("%.1f%ciB", left, units.charAt(unit));
		}
	}

	public static SwapView getSwapFor(int pid){
		try{
			String comm = new String(Files.readAllBytes(
				Paths.get(String.format("/proc/%d/cmdline", pid))),
				StandardCharsets.UTF_8);
			comm = comm.replace('\0',' ');
			if(comm.charAt(comm.length() - 1) == ' ')
				comm = comm.substring(0, comm.length() - 1);
			double s = 0;
			for(String l: Files.readAllLines(
				Paths.get(String.format("/proc/%d/smaps", pid)),
				StandardCharsets.UTF_8)){
				if(l.startsWith("Swap:")){
					String[] a=l.split(" ");
					s += Integer.parseInt(a[a.length-2]);
				}
			}
			return new SwapView(pid, s*1024, comm);
		}catch(Exception e){
			return new SwapView(pid, 0, "");
		}
	}

	public static List<SwapView> getSwap(){
		List<SwapView> ret= new ArrayList<>();
		for(File fpid: new File("/proc").listFiles()){
			try{
				int pid = Integer.parseInt(fpid.getName());
				SwapView s = getSwapFor(pid);
				if(s.size > 0) ret.add(s);
			}catch(NumberFormatException e){} // do nothing for parse error
		}
		ret.sort((a, b) -> Double.compare(a.size, b.size));
		return ret;
	}

	public static void main (String [] argv){
		List<SwapView> results = getSwap();
		System.out.printf("%7s %9s %s\n", "PID", "SWAP", "COMMAND");
		double t=0.0;
		for(SwapView s: results){
			System.out.printf("%7d %9s %s\n", s.pid, filesize(s.size), s.comm);
			t += s.size;
		}
		System.out.printf("Total: %10s\n", filesize(t));
	}
}
