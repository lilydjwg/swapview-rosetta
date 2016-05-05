#!/usr/bin/env node

const coreCount = 4;
const fs = require('fs');
const sprintf = require('sprintf');
const Parallel = require('paralleljs');

function filesize(size) {
  const units = 'KMGT';
  let left = size;
  let unit = -1;
  for (; left > 1100 && unit < 3; unit++) {
    left /= 1024;
  }
  if (unit === -1) {
    return sprintf('%dB', size);
  } else {
    if (size < 0) left = -left;
    return sprintf('%.1f%siB', left, units[unit]);
  }
}

function getSwapForChunk(pids) {
  const fs = require('fs');
  return pids.map(function(pid) {
    try {
      let comm = fs.readFileSync('/proc/' + pid + '/cmdline', 'utf-8');
      if(comm[comm.length-1] === '\0'){
        comm = comm.slice(0, -1)
      }
      comm = comm.replace(/\0/g,' ');
      let s = 0;
      const smaps = fs.readFileSync('/proc/'+ pid +'/smaps', 'utf-8');
      const re = /\nSwap:\s+(\d+)/g;
      let result;
      while (result = re.exec(smaps)) {
        s += result[1] | 0;
      }
      return [pid, s*1024, comm];
    } catch(e) {
      return [pid, 0, ''];
    }
  }).filter(function(p) {
    return p[1] > 0;
  })
}

function splitArrayToChunks(chunkCount, arr) {
  if (chunkCount < 2) {
    return [arr];
  }

  const chunks = []; // result chunks
  const length = arr.length;
  let chunkSize;
  let i; // loop variable

  if (length % chunkCount === 0) {
    chunkSize = Math.floor(length / chunkCount);
    i = 0;
    while (i < length) {
      chunks.push(arr.slice(i, i += chunkSize));
    }
  } else {
    i = 0;
    while (i < length) {
      chunkSize = Math.ceil((length - i) / chunkCount--);
      chunks.push(arr.slice(i, i += chunkSize));
    }
  }
  return chunks;
}

const splitData = splitArrayToChunks.bind(null, coreCount);

function getSwap(callback) {
  var ret = [];
  var pids = [];
  fs.readdirSync("/proc").forEach(function(spid) {
    var pid = spid|0;
    if (pid > 0) {
      pids.push(pid);
    }
  });

  // split pids into balanced chunks and pass to each worker
  var p = new Parallel(splitData(pids), { maxWorkers: coreCount });
  p.map(getSwapForChunk).then(function(chunks) {
    callback(
      Array.prototype.concat.apply([], chunks)
        .sort(function(a, b) { return a[1] - b[1]; })
    )
  });
}


function view(results) {
  console.log(sprintf('%5s %9s %s', 'PID', 'SWAP', 'COMMAND'));
  var t = 0;
  results.forEach(function(s) {
    console.log(sprintf('%5s %9s %s', s[0], filesize(s[1]), s[2]));
    t += s[1];
  });
  console.log(sprintf('Total: %8s', filesize(t)));
}

getSwap(view);
