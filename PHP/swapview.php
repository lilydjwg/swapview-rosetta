#!/usr/bin/env php
<?php

function renderSize($size)
{
    $units = "KMGT";
    $left = $size;
    $unit = -1;

    while ($left > 1100 && $unit < 3) {
        $left /= 1024;
        $unit++;
    }

    if ($unit === -1) {
        return sprintf("%dB", $size);
    } else {
        if ($size < 0) {
            $left = -1 * $left;
        }

        return sprintf('%.1f%siB', $left, $units[$unit]);
    }
}


function getSwapFor($pid)
{
    $comm = file_get_contents("/proc/$pid/cmdline", "r");
    if ("\0" == substr($comm, -1)) {
        $comm = substr($comm, 0, strlen($comm) - 1);
    }
    $comm = str_replace("\0", " ", $comm);

    $s = 0;
    $smaps = file_get_contents("/proc/$pid/smaps", "r");
    $matchCount = preg_match_all('/\nSwap:\s+(\d+)/', $smaps, $matches);

    if (false === $matchCount) {
        return array($pid, 0, '');

    } else {
        foreach ($matches[1] as $match) {
            $s += $match;
        }

        return array($pid, $s * 1024, $comm);
    }
}


function getSwap()
{
    $ret = array();

    $dir = dir('/proc');
    while (false !== ($file = $dir->read())) {
        $pid = intval($file);
        if (0 < $pid) {
            $swap = getSwapFor($pid);
            if (0 < $swap[1]) {
                $ret[] = $swap;
            }
        }
    }

    usort($ret, function ($a, $b) {
        return $a[1] - $b[1];
    });

    return $ret;
}


$results = getSwap();
printf("%5s %9s %s", "PID", "SWAP", "COMMAND" . PHP_EOL);

$totalSize = 0;
foreach ($results as $result) {
    printf("%5s %9s %s" . PHP_EOL, $result[0], renderSize($result[1]),
        $result[2]);
    $totalSize += $result[1];
}

printf("Total: %8s" . PHP_EOL, renderSize($totalSize));
