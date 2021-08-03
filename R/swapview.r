#!/usr/bin/env Rscript

filesize <- function (size){
  left = abs(size)
  unit = 0
  units = unlist(strsplit("KMGT",""))
  while ( left > 1100 && unit < 4){
    left = left / 1024
    unit = unit + 1
  }
  ret = ""
  if( unit == 0 ){
    sprintf("%dB", size)
  }else{
    sprintf("%.1f%siB", left, units[unit])
  }
}

getComm <- function (pid){
  fcomm = file(sprintf('/proc/%d/cmdline', pid), "rb")
  on.exit(close(fcomm))
  comm = NULL
  rawspace = charToRaw(" ")
  while(length(cbin <- readBin(fcomm, raw())) != 0){
    comm = c(comm, if(cbin[1] == raw(1)) rawspace else cbin[1])
  }
  comm = rawToChar(comm[1:length(comm) - (if(comm[length(comm)]==rawspace) 1 else 0)])
  comm
}

getSize <- function(pid){
  fsmaps = file(sprintf('/proc/%d/smaps', pid), "rb")
  on.exit(close(fsmaps))
  smaps = readLines(fsmaps)
  sum(sapply(smaps[grepl("Swap:", smaps)],
             function(a) as.numeric(rev(unlist(strsplit(a, " ")[[1]]))[2]))
      )*1024
}

getSwapFor <- function(pid){
  tryCatch(list(pid, getSize(pid), getComm(pid)),
           error=function(e) list(pid, 0, ""))
}

getSwap <- function(){
  all = sapply(sapply(list.files("/proc", pattern="^[0-9]+$"), strtoi), getSwapFor)
  all = all[, all[2,] > 0]
  all = data.frame(pid=unlist(unname(all[1,])), 
                   size=unlist(unname(all[2,])), 
                   comm=unlist(unname(all[3,])))
  if(length(all)>0){
      all = all[order(all[,2]),]
  }
  all
}

result = getSwap()
write(sprintf("%7s %9s %s", "PID", "SWAP", "COMMAND"), stdout())
t = 0
for(i in 1:nrow(result)){
  size = as.numeric(result[i,2])
  if(length(size)>0){
    write(sprintf("%7s %9s %s", result[i,1], filesize(size), result[i,3]), stdout())
    t = t + size
  }
}
write(sprintf("Total: %10s", filesize(t)), stdout())
