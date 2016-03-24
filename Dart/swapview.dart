import 'dart:io';
import 'package:path/path.dart';

class SwapInfo implements Comparable {
  String pid;
  int swap;
  String cmdline;

  SwapInfo(this.pid, this.swap, this.cmdline);
  int compareTo(SwapInfo other) => this.swap.compareTo(other.swap);
}

bool isDigit(String s) {
  return s.codeUnits.every((int element) => (element ^ 0x30) <= 9);
}

toHumanReadable(int size) {
  final units = "KMGT";
  var unit = -1;
  var left = size;
  while (left > 1100 && unit < 3) {
    left /= 1024;
    unit++;
  }
  if (unit == -1) {
    return "${size}B";
  } else {
    return "${left.toStringAsFixed(1)}${units[unit]}iB";
  }
}

getSwapFor(String pid) {
  var swap = 0;
  var cmdline = '';
  final regex = new RegExp(r"Swap:\s+(\d+)");
  try {
    cmdline = new File('/proc/${pid}/cmdline')
        .readAsStringSync()
        .replaceAll('\x00', ' ')
        .trim();
    var smaps = new File('/proc/${pid}/smaps').readAsStringSync().split('\n');
    for (String line in smaps.where((l) => l.startsWith('Swap:'))) {
      swap += int.parse(regex.firstMatch(line).group(1));
    }
  } on FileSystemException {
    return new SwapInfo(pid, 0, '');
  }
  return new SwapInfo(pid, swap * 1024, cmdline);
}

main() {
  var procDir = new Directory('/proc');
  var processes = procDir.listSync();
  var total = 0;
  var results = new List.from(processes
      .map((fse) => basename(fse.path))
      .where((pid) => isDigit(pid))
      .map((pid) => getSwapFor(pid))
      .where((info) => info.swap != 0));
  results.sort();

  print("${"PID".padLeft(5)} ${"SWAP".padLeft(9)} COMMAND");
  for (SwapInfo result in results) {
    total += result.swap;
    print(
        "${result.pid.padLeft(5)} ${toHumanReadable(result.swap).padLeft(9)} ${result.cmdline}");
  }
  print("Total: ${toHumanReadable(total).padLeft(8)}");
}
