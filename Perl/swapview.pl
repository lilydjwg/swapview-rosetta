#!/usr/bin/env perl

use strict;
use warnings;

sub filesize{
    my ($size) = @_;
    my $units = "KMGT";
    my $left = abs($size);
    my $unit = -1;
    while($left > 1100 and $unit < 3){
        $left /= 1024;
        $unit++;
    }
    if($unit == -1){
        return sprintf('%dB', $size);
    }else{
        if($size < 0){
            $left = - $left; 
        }
        return sprintf("%.1f%siB", $left, substr($units, $unit, 1));
    }
}

sub getSwapFor{
    my ($pid) = @_;
    open FILE,sprintf("/proc/%s/cmdline", $pid) or return ($pid, 0, '');
    my $comm = join("", <FILE>); 
    close FILE;
    substr($comm, length($comm) - 1, 1) eq "\0" and chop($comm);
    $comm =~ s/\0/ /g;
    open FILE,sprintf("/proc/%s/smaps", $pid) or return ($pid, 0, '');
    my $s=0;
    while(<FILE>){
        if(/^Swap:/){
            s/[^0-9]//g;
            $s += $_;
        }
    }
    close FILE;
    return (pid => $pid, size => $s * 1024, comm =>$comm);
}

sub getSwap{
    my @ret = ();
    opendir (DIR,'/proc') or die $!;
    while (readdir(DIR)){
        if(/^[0-9]+$/){
            my %s = getSwapFor($_);
            if($s{size} > 0){
                push @ret, {%s};
            }
        }
    }
    my @sort_ret = sort { $a->{size} <=> $b->{size} } @ret;
    return @sort_ret;
}

my @result = getSwap();
printf "%5s %9s %s\n", 'PID', 'SWAP', 'COMMAND';
my $t = 0;
for my $i (@result){
    print sprintf("%5s %9s %s\n", $i->{pid}, filesize($i->{size}), $i->{comm});
    $t += $i->{size};
}
print sprintf("Total: %8s\n", filesize($t));
