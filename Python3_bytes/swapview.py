#!/usr/bin/env python3

import os

format = "%7s %9s %s"
totalFmt = "Total: %8s"

def filesize(size):
  units = 'KMGT'
  left = abs(size)
  unit = -1
  while left > 1100 and unit < 3:
    left = left / 1024
    unit += 1
  if unit == -1:
    return '%dB' % size
  else:
    if size < 0:
      left = -left
    return '%.1f%siB' % (left, units[unit])

def getSwapFor(pid):
  try:
    comm = open('/proc/%s/cmdline' % pid, 'rb').read()
    if comm.endswith(b'\x00'):
      comm = comm[:-1]
    comm = comm.replace(b'\x00', b' ')
    with open('/proc/%s/smaps' % pid, 'rb') as f:
      s = sum(int(l.split()[1].decode()) for l in f if l.startswith(b'Swap:'))
    return pid, s * 1024, comm.decode()
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
