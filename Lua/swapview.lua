#!/usr/bin/env lua
require "lfs"
function filesize(size)
	--将数字转换为 xxKiB 的形式
	local units = { "K", "M", "G", "T" }
	local value = math.abs(size)
	local uint = 0
	while value > 1100 and uint < 4 do
		value = value / 1024
		uint = uint + 1
	end
	if uint == 0 then
		return string.format("%dB", size)
	else
		if size < 0 then
			value = -value
		end
		return string.format("%f%siB", value, units[uint])
	end
end

function getSwapFor(pid)
	local size = 0
	local swapfile = io.open(string.format("/proc/%s/smaps", pid), "r")
	if swapfile then
		for line in swapfile:lines() do
			if string.sub(line, 1, 5) == "Swap:" then
				size = size + tonumber(string.match(line, "%d+$") or 0)
			end
		end
	end
	swapfile:close()
	local cmd = ""
	local cmdfile = io.open(string.format("/proc/%s/cmdline", pid), "r")
	if cmdfile then
		cmd = string.gsub(cmdfile:read("*a"), "\x00", " ")
		--cmd = cmdfile:read("*a")
	end
	cmdfile:close()
	return size, cmd
end

function getSwap()
	local ret = {}
	for pid in lfs.dir('/proc') do
		if tonumber(pid) then
			size, cmd = getSwapFor(pid)
			if size > 0 then
				table.insert(ret, { pid, size, cmd })
			end
		end
	end
	local comp = function(a, b)
		if a[2] < b[2] then
			return true
		end
	end
	table.sort(ret, comp)
	return ret
end

io.write(string.format("%5s %9s %s\n", "PID", "SWAP", "COMMAND"))
local results = getSwap()
local sum = 0
for _, v in ipairs(results) do
	io.write(string.format("%5s %9s %s\n", v[1], filesize(v[2]), v[3]))
	sum = sum + v[2]
end
io.write(string.format("Total: %8s\n", filesize(sum)))
