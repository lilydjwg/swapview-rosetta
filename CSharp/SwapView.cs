using System;
using System.IO;
using System.Collections.Generic;

namespace SwapView
{
	public struct SwapView: IComparable<SwapView>
	{
		public int pid;
		public double size; 
		public string comm;

		public SwapView(int p, double s, string c)
		{
			pid = p;
			size = s;
			comm = c;
		}

		public int CompareTo(SwapView s)
		{
			return size.CompareTo (s.size);
		}
	}

	class MainClass
	{
		public static string filesize(double size)
		{
			string units = "KMGT";
			double left = Math.Abs (size);
			int unit = -1;
			while(left > 1100 && unit < 3){
				left /= 1024;
				unit++;
			}
			if(unit == -1){
				return String.Format ("{0:D}B", (int)size);
			}else{
				if(size < 0) left = -left;
				return String.Format ("{0:F1}{1:C}iB", left, units[unit]);
			}
		}

		public static SwapView getSwapFor(int pid)
		{
			try{
				string comm = File.ReadAllText (String.Format ("/proc/{0:D}/cmdline", pid));
				comm = comm.Replace('\0',' ');
				if(comm[comm.Length - 1] == ' ')
					comm = comm.Substring(0, comm.Length - 1);
				double s = 0;
				foreach(string l in File.ReadLines (String.Format ("/proc/{0:D}/smaps", pid))){
					if(l.StartsWith("Swap:")){
						string[] a=l.Split(' ');
						s += int.Parse(a[a.Length-2]);
					}
				}
				return new SwapView(pid, s*1024, comm);
			}catch(Exception){
				return new SwapView(pid, 0, "");
			}
		}

		public static IList<SwapView> getSwap()
		{
			List<SwapView> ret = new List<SwapView>();
			foreach(string fpid in Directory.EnumerateDirectories ("/proc/")){
				int pid;
				if(int.TryParse(fpid.Substring (6), out pid)){
					SwapView s = getSwapFor (pid);
					if(s.size > 0) ret.Add (s);
				}
			}
			ret.Sort ();
			return ret;
		}

		public static void Main (string[] args)
		{
			IList<SwapView> results = getSwap ();
			Console.WriteLine ("{0,7} {1,9} {2}", "PID", "SWAP", "COMMAND");
			double t=0.0;
			foreach(SwapView s in results){
				Console.WriteLine ("{0,7:D} {1,9} {2}", s.pid, filesize (s.size), s.comm);
				t += s.size;
			}
			Console.WriteLine ("Total: {0,10}", filesize (t));
		}
	}
}
