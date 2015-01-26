#!/usr/bin/coffee

fs = require 'fs'
Promise = require 'promise'

readdir = Promise.denodeify fs.readdir
read = Promise.denodeify fs.readFile

addString = (sum, line) -> sum + (Number (line.match /\d+/)[0])
addSecond = (sum, aaa) -> sum + aaa[1]

formatSize = (acc, n) ->
  if (n > 1100) and (acc <= 3)
  then formatSize (acc + 1), (n /= 1024)
  else
    unit = 'KMGT'[acc]
    num = n.toFixed(1)
    "#{num}#{unit}iB"
formatSize0 = (n) -> formatSize 0, n

fillLength = (n, str) ->
  if str.length < n
  then fillLength n, " #{str}"
  else str

readdir('/proc')
.then (files) ->
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
    .filter (aaa) -> aaa[1].length > 0
    .map (aaa) ->
      aaa[1] = aaa[1].reduce(addString, 0)
      aaa
    .filter (aaa) -> aaa[1] > 0
    .sort (aaa, bbb) -> aaa[1] - bbb[1]
    output out
.then null, (error) ->
  console.error error.stack
  process.exit 1

output = (res) ->
  console.log [
    fillLength 5, 'PID'
    fillLength 9, 'SWAP'
    'COMMAND'
  ].join(' ')

  str = res
  .map (aaa) ->
    [
      fillLength 5, aaa[0]
      fillLength 9, (formatSize0 aaa[1])
      aaa[2]
    ].join(' ')
  .join('\n')
  console.log str

  console.log [
    'Total:'
    fillLength 9, (formatSize0 (res.reduce addSecond, 0))
  ].join('')
