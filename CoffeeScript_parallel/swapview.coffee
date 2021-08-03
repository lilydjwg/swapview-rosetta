#!/usr/bin/coffee
# This version decouples the smap and cmdline reading into two independent
# sets of promises. No threads are involved.
"use strict"
fs = require 'fs'
denodeify = require 'denodeify'

readdir = denodeify fs.readdir
read = denodeify fs.readFile

addString = (sum, line) -> sum + (Number (line.match /\d+/)[0])
addSecond = (sum, x) -> sum + x[1]

formatSize = (acc, n) ->
  if (n > 1100) and (acc <= 3)
  then formatSize (acc + 1), (n /= 1024)
  else
    unit = 'KMGT'[acc]
    num = n.toFixed(1)
    "#{num}#{unit}iB"
formatSize0 = (n) -> formatSize 0, n

fillLength = (n, str) ->
  str.padStart n, ' '

readdir('/proc')
.then (files) ->
  ###*
  * @type Promise<string[3]>  pid, swapsize, cmdline
  ###
  list = files
  .filter (file) -> file.match /\d+/
  .map (file) ->
    readSmaps = (read "/proc/#{file}/smaps", 'ascii')
    .then (text) ->
      text.split('\n').filter (line) -> line[..4] is 'Swap:'
    readCmdline = (read "/proc/#{file}/cmdline", 'utf-8')
    .then (text) ->
      cmdline = text
      .replace(/\0$/, '')
      .replace(/\0/g, ' ')
      cmdline
    (Promise.all [readSmaps, readCmdline])
    .then (pair) ->
      [file].concat pair
    .then null, (error) ->
      [file, [], '']
  (Promise.all list)
  .then (res) ->
    out = res
    .filter (swapsize) -> swapsize[1].length > 0
    .map (procEntry) ->
      procEntry[1] = procEntry[1].reduce(addString, 0)
      procEntry
    .filter (procEntry) -> procEntry[1] > 0
    .sort (a, b) -> a[1] - b[1]
    prettyPrint out
.then null, (error) ->
  console.error error.stack
  process.exit 1

prettyPrint = (res) ->
  console.log [
    fillLength 7, 'PID'
    fillLength 9, 'SWAP'
    'COMMAND'
  ].join(' ')

  str = res
  .map (procEntry) ->
    [
      fillLength 7, procEntry[0]
      fillLength 9, (formatSize0 procEntry[1])
      procEntry[2]
    ].join(' ')
  .join('\n')
  console.log str

  console.log [
    'Total: '
    fillLength 10, (formatSize0 (res.reduce addSecond, 0))
  ].join('')
