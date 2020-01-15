#!/usr/bin/env node
"use strict"
const fs = require('fs');
const cluster = require('cluster');

if (cluster.isMaster) {
  ////////////////////
  //  Master process
  ////////////////////

  const coreCount = require('os').cpus().length * 4;
  const sprintf = require('sprintf-js').sprintf;

  const filesize = function(size) {
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

  const splitArrayToChunks = function(chunkCount, arr) {
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

  const getPids = function() {
    const ret = [];
    const pids = [];
    fs.readdirSync("/proc").forEach(function(spid) {
      const pid = spid|0;
      if (pid > 0) {
        pids.push(pid);
      }
    });

    return pids;
  }

  const printSwapChunks = function(chunks) {
    const results = Array.prototype.concat.apply([], chunks)
    .sort(function(a, b) { return a[1] - b[1]; })
    console.log(sprintf('%7s %9s %s', 'PID', 'SWAP', 'COMMAND'));
    let t = 0;
    results.forEach(function(s) {
      console.log(sprintf('%7s %9s %s', s[0], filesize(s[1]), s[2]));
      t += s[1];
    });
    console.log(sprintf('Total: %8s', filesize(t)));
  }

  // fork workers
  for (let i = 0; i < coreCount; i++) {
    cluster.fork();
  }

  const pidChunks = splitArrayToChunks(coreCount, getPids());
  const swapChunks = [];

  const messageHandler = function(worker, message) {
    if (message.type === 'chunk_swap') {
      swapChunks.push(message.chunk);

      // all chunks ok
      if (swapChunks.length === coreCount) {
        // disconnect all workers.
        cluster.disconnect();

        // print sorted swap info
        printSwapChunks(swapChunks);
      }
    }
  }

  cluster.on('message', messageHandler);

  // pass pid chunks to each worker
  Object.keys(cluster.workers).forEach((workerId, index) => {
    cluster.workers[workerId].send({type: 'chunk_pid', chunk: pidChunks[index]});
  });

} else if (cluster.isWorker) {
  ////////////////////
  //  Worker process
  ////////////////////

  const getSwapForChunk = function(pids) {
    const fs = require('fs');
    return pids.map(function(pid) { // split this func
      try {
        let comm = fs.readFileSync('/proc/' + pid + '/cmdline', 'utf-8');
        if(comm[comm.length-1] === '\0') {
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

  const messageHandler = (message) => {
    if (message.type === 'chunk_pid') {
      process.send({type: 'chunk_swap', chunk: getSwapForChunk(message.chunk)});
    }
  }

  process.on('message', messageHandler);
}
