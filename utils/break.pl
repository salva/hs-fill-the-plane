#!/usr/bin/perl

use utf8;
use strict;
use warnings;

my $dir = "/tmp/chunks";

my $data = do { local $/; <> };
my @regions = split /(^λ>.*$)/m, $data;
s/^\s+//,s/\s+$// for @regions;
@regions = grep /<\?xml/, @regions;

#use Data::Dumper;
#print Dumper [@regions];

#my @svgs = split /(?=<\?xml )/, $regions[-1];
my @svgs = $regions[-1] =~ m|<\?xml.*?</svg>|gms;
system "rm -Rf $dir";
mkdir $dir;
for my $i (0..$#svgs) {
    my $svg = $svgs[$i] // next;
    open my $fh, ">$dir/tree-$i.svg";
    print $fh $svg;
    close $fh
}

system "eog $dir/tre*.svg &" if @svgs;

