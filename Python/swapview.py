#!/usr/bin/env python3

from __future__ import print_function
from __future__ import division

import os
import re

format = "%5s %9s %s"
totalFmt = "Total: %8s"
swap_size_pattern = re.compile(r'Swap:\s+([^\s]+)')

def filesize(size):
  units = 'KMGT'
  left = abs(size)
  unit = -1
  while left > 1100 and unit < 3:
    left /= 1024
    unit += 1
  if unit == -1:
    return '%dB' % size
  else:
    return '%.1f%siB' % (left if size > 0 else -left, units[unit])

def getSwapFor(pid):
  try:
    comm = open('/proc/%s/cmdline' % pid).read()
    if comm and comm[-1] == '\x00':
      comm = comm[:-1]
    comm = comm.replace('\x00', ' ')
    s = sum(int(i.group(1)) for i in swap_size_pattern.finditer(open('/proc/%s/smaps' % pid).read()))
    return pid, s * 1024, comm
  except (IOError, OSError):
    return pid, 0, ''

def getSwap():
  ret = []
  for pid in os.listdir('/proc'):
    if pid.isdigit():
      s = getSwapFor(pid)
      if s[1] > 0:
        ret.append(s)
  ret.sort(key=lambda x: x[1])
  return ret

def main():
  results = getSwap()
  print(format % ('PID', 'SWAP', 'COMMAND'))
  for pid, swap, comm in results:
    print(format % (pid, filesize(swap), comm))
  t = filesize(sum(x[1] for x in results))
  print(totalFmt % t)

if __name__ == '__main__':
  main()
