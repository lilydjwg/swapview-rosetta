#!/usr/bin/pwsh -nop

$ErrorActionPreference= 'SilentlyContinue'

function FileSize($size) {
    if ($size -gt 1GB) {
        "{0}GiB" -f [Math]::Round($size / 1GB, 1)
    } elseif ($size -gt 1MB) {
        "{0}MiB" -f [Math]::Round($size / 1MB, 1)
    } else {
        "{0}KiB" -f [Math]::Round($size / 1KB, 1)
    }
}

function GetSwapFor($_pid) {
    $sum = ForEach-Object -InputObject (Select-String -Path /proc/$_pid/smaps -Pattern "(?<=Swap: +)\d+") { $_.Matches.Value } | Measure-Object -Sum
    $sum.Sum * 1KB
}

$total = 0

Get-ChildItem /proc | Where-Object { $_.Name -match '^[0-9]+$' } | ForEach-Object {
    $cmd = Get-Content -Path /proc/$($_.Name)/cmdline
    if ($cmd -eq $null)  {
        return
    }
    $size = GetSwapFor $_.Name
    
    $total += $size
    [PSCustomObject]@{
        "Pid"  = $_.Name
        "Size" = $size
        "Cmd"  = $cmd
    }
} | Sort-Object -Property Size | ForEach-Object {
    Write-Output ("{0,7} {1,9} {2}" -f $_.Pid, (FileSize $_.Size), $_.Cmd)
}

Write-Output ("Total: {0,8}" -f (FileSize $total))
