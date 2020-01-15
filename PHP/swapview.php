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
    $fallback = [$pid, 0, ''];
    $commFile = '/proc/' . $pid . '/cmdline';
    if(!($comm = @file_get_contents($commFile))) {
        return $fallback;
    }
    if ("\0" == substr($comm, -1)) {
        $comm = substr($comm, 0, strlen($comm) - 1);
    }

    $comm = str_replace("\0", " ", $comm);
    $smapsFile = '/proc/' . $pid . '/smaps';
    if (!($smaps = @file_get_contents($smapsFile))) {
        return $fallback;
    }

    $matchCount = preg_match_all('/\nSwap:\s+(\d+)/', $smaps, $matches);
    if (!$matchCount) {
        return $fallback;
    } else {
        $sum = array_sum($matches[1]);
        return array($pid, $sum * 1024, $comm);
    }
}


function getSwap()
{
    $ret = [];
    $dirs = scandir('/proc');
    foreach($dirs as $item)
    {
        $pid = intval($item);
        if($pid)
        {
            $swap = getSwapFor($pid);
            if($swap[1])
            {
                $ret[] = $swap;
            }
        }
    }
    array_multisort(array_column($ret, 1), SORT_ASC, $ret);
    return $ret;
}


$results = getSwap();
printf("%7s %9s %s", "PID", "SWAP", "COMMAND" . PHP_EOL);

$totalSize = 0;
foreach ($results as $result) {
    printf("%7s %9s %s" . PHP_EOL, $result[0], renderSize($result[1]), $result[2]);
    $totalSize += $result[1];
}

printf("Total: %8s" . PHP_EOL, renderSize($totalSize));
