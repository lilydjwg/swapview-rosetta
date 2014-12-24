#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <cmath>
#include <iomanip>
#include <tuple>
#include <algorithm>
#include <vector>
using namespace std;

template<typename... UArgs>
class StrFormatHelper{
	ostringstream sout;
	template<typename T>
	void format(T value){ sout<<value; }

	template<typename T, typename... TArgs>
	void format(T value, TArgs... args){
		sout<<value;
		format(args...);
	}

public:
	StrFormatHelper(UArgs... args){ format(args...); }

	operator string (){ return sout.str(); }
};

template<typename... UArgs>
string strformat(UArgs... args){
	return StrFormatHelper<UArgs...>(args...);
}


string filesize(auto size){
	char units [] = "KMGT";
	auto left = fabs(size);
	int unit = -1;
	while( left > 1100 && unit < 3 ){
		left /= 1024;
		unit++;
	}
	if(unit == -1){
		return strformat(static_cast<int>(size), 'B');
	}else{
		if(size<0){
			left = -left;
		}
		return strformat(fixed, setprecision(1) , left , units[unit] , 'B');
		
	}
}

vector<string> readlines(string path){
	ifstream fcomm(path);
	vector<string> result;
	for(string buf; getline(fcomm, buf);){
		result.push_back(string(buf));
	}
	return result;
}
// def getSwapFor(pid):
//   try:
//     comm = open('/proc/%s/cmdline' % pid).read().replace('\x00', ' ')
//     s = 0
//     for l in open('/proc/%s/smaps' % pid):
//       if l.startswith('Size:'):
//         s += int(re.search(r'\d+', l).group(0))
//     return pid, s * 1024, comm[:-1]
//   except (IOError, OSError):
//     return pid, 0, ''

tuple<int, double, string> getSwapFor(int pid){
	try{
		string comm = readlines(strformat("/proc/", pid, "/cmdline"))[0];
		replace(comm.begin(), comm.end(), '\0' , ' ');
		return make_tuple(pid, 0.0, comm);
	}catch(...){
		return make_tuple(pid, 0.0, ""s);
	}
}

// def getSwap():
//   ret = []
//   for pid in os.listdir('/proc'):
//     if pid.isdigit():
//       s = getSwapFor(pid)
//       if s[1] > 0:
//         ret.append(s)
//   ret.sort(key=lambda x: x[1])
//   return ret

void format_print(auto pid, auto swap, auto command){
	cout<<setw(5)<<pid<<" "<<setw(9)<<swap<<" "<<command<<endl;
}

void format_print(tuple<int, double, string> swap){
	format_print(get<0>(swap), get<1>(swap), get<2>(swap));
}

int main(int argc, char * argv[]){
	format_print("PID", "SWAP", "COMMAND");
	format_print(getSwapFor(1));
	return 0;
}
