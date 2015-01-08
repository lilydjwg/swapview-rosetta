#!/usr/bin/env node

var sprintf = require("sprintf");
var fs = require("fs");

function filesize(size) {
  var units = "KMGT";
  var left = size;
  var unit = -1;
  for (; left > 1100 && unit < 3; unit++) {
    left /= 1024;
  }
  if (unit === -1) {
    return sprintf("%dB", size);
  } else {
    if (size < 0) left = -left;
    return sprintf('%.1f%siB', left, units[unit]);
  }
}

function getSwapFor(pid){
  try{
    var comm = fs.readFileSync("/proc/"+pid+"/cmdline", "utf-8");
    if(comm[comm.length-1] === '\0'){
      comm = comm.slice(0, -1)
    }
    comm = comm.replace(/\0/g," ");
    var s=0;
    var smaps = fs.readFileSync("/proc/"+pid+"/smaps", "utf-8");
    var re=/\nSwap:\s+(\d+)/g,result;
    while (result=re.exec(smaps)) {
      s+=result[1]|0;
    }
    return [pid, s*1024, comm];
  } catch(e) {
    return [pid, 0, ""];
  }
}

function getSwap() {
  var ret = [];
  fs.readdirSync("/proc").forEach(function(spid) {
    var pid = spid|0;
    if (pid > 0) {
      var s = getSwapFor(pid);
      if (s[1] > 0) ret.push(s);
    }
  });
  ret.sort(function(a, b) {
    return a[1] - b[1];
  })
  return ret;
}

var results = getSwap();
console.log(sprintf("%5s %9s %s", "PID", "SWAP", "COMMAND"));
var t = 0;
results.forEach(function(s) {
  console.log(sprintf("%5s %9s %s", s[0], filesize(s[1]), s[2]));
  t += s[1];
});
console.log(sprintf("Total: %8s", filesize(t)));
