#!/usr/bin/env perl

use strict;
use warnings;
use utf8;
use autodie;
use 5.010;
use threads;
use threads::shared;
use Thread::Queue;

# Avoids regex performance penalty in perl 5.16 and earlier.
use English qw{ -no_match_vars };

my $results = Thread::Queue->new();
my $count : shared = 0;

sub get_process_swap ($) {
    my $q = shift;
    while (1) {
        my $pid = $q->dequeue();
        last unless ($pid);
        my $smaps_file = join q{/}, $pid, q{smaps};
        my $swap_size  = 0;
        if (    -f $smaps_file
            and -r $smaps_file
            and ( $EUID == 0 or -O $smaps_file ) )
        {
            open my $smaps_file_fh, '<', $smaps_file;
            while ( my $line = <$smaps_file_fh> ) {
                if ( $line =~ /Swap:/ ) {
                    $swap_size += ( split( /[[:space:]]+/, $line ) )[1];
                }
            }
            close $smaps_file_fh;
        }
        next if ( $swap_size == 0 );
        my $cmdline_file = join q{/}, $pid, q{cmdline};
        my $cmdline;
        if (    -f $cmdline_file
            and -r $cmdline_file
            and ( $EUID == 0 or -O $cmdline_file ) )
        {
            open my $cmdline_file_fh, '<', $cmdline_file;
            $cmdline = do {
                local $RS;
                <$cmdline_file_fh>;
            };
            close $cmdline_file_fh;
            $cmdline =~ s/\0$//;
            $cmdline =~ s/\0/ /g;
        }
        {
            lock($count);
            $count = $count + 1;
        }
        $pid = $pid =~ s/\D+//r;
        $results->enqueue(
            { pid => $pid, swap => $swap_size, cmd => $cmdline, } );
    }

}

sub get_swap_info_from ($) {
    my $q    = Thread::Queue->new();
    my $cpus = do { local @ARGV = '/proc/cpuinfo'; grep /^processor\s+:/, <>; };
    foreach ( 1 .. $cpus ) {
        threads->create( \&get_process_swap, $q );
    }
    $q->enqueue($_) foreach ( @{ $_[0] } );
    $q->end();
    $_->join() foreach ( threads->list() );
    if ($count) {
        return $results->extract( 0, $count );
    }
    else {
        return ();
    }
}

sub convert_file_size_from_kB ($) {
    my $size  = shift;
    my @units = qw{K M G T};
    my $unit  = 0;
    while ( $size > 1100 and $unit < 3 ) {
        $size /= 1024;
        $unit += 1;
    }
    return sprintf( "%.1f%siB", $size, $units[$unit] );
}

sub main {
    my $printf_format = qq{%7s %9s %s\n};
    my @pid_dirs      = glob "/proc/[0123456789]*";
    my @sorted_results =
      sort { $a->{'swap'} <=> $b->{'swap'} } get_swap_info_from( \@pid_dirs );
    my $total_swap_size = 0;
    printf $printf_format, qw{PID SWAP COMMAND};
    for my $result (@sorted_results) {
        $total_swap_size += $result->{'swap'};
        printf $printf_format, $result->{'pid'},
          convert_file_size_from_kB( $result->{'swap'} ), $result->{'cmd'};
    }
    printf qq{Total: %10s\n}, convert_file_size_from_kB($total_swap_size);
}

main
