#!/usr/bin/env node
"use strict"
const sprintf = require('sprintf-js').sprintf
const fs = require("fs")

function filesize(size) {
  const units = "KMGT"
  let left = size
  let unit = -1
  for (; left > 1100 && unit < 3; unit++) {
    left /= 1024
  }
  if (unit === -1) {
    return sprintf("%dB", size)
  } else {
    if (size < 0) left = -left
    return sprintf('%.1f%siB', left, units[unit])
  }
}

function getSwapFor(pid){
  try {
    let comm = fs.readFileSync(`/proc/${pid}/cmdline`, "utf-8")
    if(comm[comm.length-1] === '\0'){
      comm = comm.slice(0, -1)
    }
    comm = comm.replace(/\0/g," ")
    let s = 0
    const smaps = fs.readFileSync(`/proc/${pid}/smaps`, "utf-8")
    const re = /\nSwap:\s+(\d+)/g
    let result
    while (result = re.exec(smaps)) {
      s += result[1] | 0
    }
    return [pid, s*1024, comm]
  } catch(e) {
    if(e.code === 'EACCES' || e.code === 'ENOENT') {
      return [pid, 0, ""]
    } else {
      throw e
    }
  }
}

function getSwap() {
  const ret = []
  fs.readdirSync("/proc").forEach(spid => {
    const pid = spid | 0
    if (pid > 0) {
      const s = getSwapFor(pid)
      if (s[1] > 0) {
        ret.push(s)
      }
    }
  })
  ret.sort((a, b) => a[1] - b[1])
  return ret
}

const results = getSwap()
console.log(sprintf("%7s %9s %s", "PID", "SWAP", "COMMAND"))
let t = 0
results.forEach(s => {
  console.log(sprintf("%7s %9s %s", s[0], filesize(s[1]), s[2]))
  t += s[1]
})

console.log(sprintf("Total: %8s", filesize(t)))
