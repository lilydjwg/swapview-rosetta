#!/usr/bin/env lua

require 'lfs'

local format = string.format
local nullstr

-- http://www.freelists.org/post/luajit/BUG-stringgsub-is-not-8bit-pure,1
if jit then
  nullstr = '%z'
else
  nullstr = '\x00'
end

function filesize(size)
  local units = {'K', 'M', 'G', 'T'}
  local value = math.abs(size)
  local uint = 0
  while value > 1100 and uint < 4 do
    value = value / 1024
    uint = uint + 1
  end
  if uint == 0 then
    return format('%dB', size)
  else
    if size < 0 then
      value = -value
    end
    return format('%.1f%siB', value, units[uint])
  end
end

function getSwapFor(pid)
  local open = io.open
  local swapfile = open(format('/proc/%s/smaps', pid), 'r')
  if not swapfile then return 0, '' end

  -- file:lines() fails when it doesn't have the permissions to read the file
  -- (but does have when opening the file, e.g. /proc/1/smaps)
  local ok, size = pcall(function()
    local size = 0
    for line in swapfile:lines() do
      if line:sub(1, 5) == 'Swap:' then
        size = size + tonumber(line:match('%d+'))
      end
    end
    return size
  end)
  swapfile:close()
  if not ok then return 0, '' end

  local cmdfile = open(format('/proc/%s/cmdline', pid), 'r')
  if not cmdfile then return 0, '' end

  local cmd = cmdfile:read('*a')
  cmdfile:close()
  if cmd and cmd:byte(#cmd) == 0 then
    cmd = cmd:sub(1, #cmd-1)
  end
  cmd = cmd:gsub(nullstr, ' ')

  return size * 1024, cmd
end

function getSwap()
  local ret = {}
  local insert = table.insert
  for pid in lfs.dir('/proc') do
    if tonumber(pid) then
      size, cmd = getSwapFor(pid)
      if size > 0 then
        insert(ret, {pid, size, cmd})
      end
    end
  end
  table.sort(ret, function(a, b)
    return a[2] < b[2]
  end)
  return ret
end

local fmt = "%7s %9s %s"
local totalFmt = "Total: %10s"
local results = getSwap()
local sum = 0
print(format(fmt, 'PID', 'SWAP', 'COMMAND'))
for _, v in ipairs(results) do
  print(format(fmt, v[1], filesize(v[2]), v[3]))
  sum = sum + v[2]
end
print(format(totalFmt, filesize(sum)))
