#!/usr/bin/env python3

# cython: language_level=3

from __future__ import print_function
from __future__ import division

from libc.stdio cimport fopen, getline, fclose, FILE
from libc.stdlib cimport atoi
from libc.string cimport strncmp, memmove
import os

format = "%7s %9s %s"
totalFmt = "Total: %8s"

cdef int find_size(char *s):
  cdef char *s_tmp = s + 5
  while s_tmp[0] == b' ':
    s_tmp += 1
  memmove(s, s_tmp, s_tmp - s)
  s = s_tmp
  while s_tmp[0] != b' ':
    s_tmp += 1
  s_tmp[0] = 0
  return atoi(s)

def filesize(size_t size):
  cdef int unit
  cdef float left

  units = 'KMGT'
  left = abs(size)
  unit = -1
  while left > 1100 and unit < 3:
    left /= 1024
    unit += 1
  if unit == -1:
    return '%dB' % size
  else:
    if size < 0:
      left = -left
    return '%.1f%siB' % (left, units[unit])

def getSwapFor(int pid):
  cdef int s = 0
  cdef char * line = NULL
  cdef size_t l = 0
  cdef FILE* cfile
  cdef ssize_t read

  try:
    comm = open('/proc/%d/cmdline' % pid).read()
    if comm and comm[-1] == '\x00':
      comm = comm[:-1]
    comm = comm.replace('\x00', ' ')

    cfile = fopen(b'/proc/%d/smaps' % pid, 'rb')
    if cfile == NULL:
      raise IOError

    while True:
      read = getline(&line, &l, cfile)
      if read == -1:
        break
      if strncmp(line, b'Swap:', 5) == 0:
        s += find_size(line)
    fclose(cfile)
    return pid, s * 1024, comm
  except (IOError, OSError):
    return pid, 0, ''

def getSwap():
  ret = []
  for pid in os.listdir('/proc'):
    if pid.isdigit():
      s = getSwapFor(int(pid))
      if s[1] > 0:
        ret.append(s)
  ret.sort(key=lambda x: x[1])
  return ret

def main():
  cdef int pid, swap

  results = getSwap()
  print(format % ('PID', 'SWAP', 'COMMAND'))
  for pid, swap, comm in results:
    print(format % (pid, filesize(swap), comm))
  t = filesize(sum(x[1] for x in results))
  print(totalFmt % t)

main()
