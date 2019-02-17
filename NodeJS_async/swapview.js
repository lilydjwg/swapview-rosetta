#!/usr/bin/env node

var sprintf = require('sprintf-js').sprintf;
var fs = require("fs");
var async = require("async");

function filesize(size){
	var units = "KMGT";
	var left = Math.abs(size);
	var unit = -1;
	for(;left > 1100 && unit < 3;unit++){
		left /= 1024;
	}
	if(unit==-1){
		return sprintf("%dB", size);
	}else{
		if(size<0) left= -left;
		return sprintf('%.1f%siB', left, units[unit]);
	}
}


function getSwapFor(pid, callback){
	async.map(["cmdline", "smaps"],
	function(name, c){
		fs.readFile("/proc/"+pid+"/"+name, encoding="utf-8", c);
	},
	function(err, results){
		if(err){ 
			callback(null, null);
			return; 
		}
		var comm = results[0];
                if(comm[comm.length-1] == '\0'){
                  comm = comm.substr(0, comm.length - 1)
                }
		comm = comm.replace(/\0/g, " ");
		var s=0.0;
		var smaps = results[1];
		smaps.split(/\n/).forEach(function (l){
			if(l.substr(0,5) == "Swap:"){
				s += parseInt(l.split(/[ ]+/)[1]);
			}
		});
		if(s>0){
			callback(null, [pid, s*1024, comm]);
		}else{
			callback(null, null);
		}
	});
}

function getSwap(callback){
	fs.readdir("/proc", function(err, data){
		async.map(data, function(spid, callback){
			var pid = parseInt(spid);
			if(pid > 0){
				getSwapFor(pid, callback);
			}else{
				callback(null, null);
			}
		}, function(err, results) {
			results = results.filter(function (x) {return x!=null;});
			results.sort(function (a, b){return a[1] - b[1];})
			callback(null, results);
		});
	});
}

getSwap(function(err, results){
	console.log(sprintf("%5s %9s %s", "PID", "SWAP", "COMMAND"));
	var t=0.0;
	results.forEach(function (s){
		console.log(sprintf("%5s %9s %s", s[0], filesize(s[1]), s[2]));
		t += s[1];
	});
	console.log(sprintf("Total: %8s", filesize(t)));
});
