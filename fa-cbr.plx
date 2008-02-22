#! /usr/bin/perl

# This script will take the number of an episode of Warren Ellis
# and Paul Duffield's FreakAngels weekly online comic and bundle 
# the pages into a Comic Book Archive file on your local system.

# Written by adoring fan Dino Morelli <dino@ui3.info>
# 2008-02-22

# This is, of course, free software. Give it to your friends. Enjoy

# Required for use: UNIX-like system with perl, wget, tar, rm

use strict;
use warnings;
use Cwd;


# Check the incoming episode number argument

my $episode = shift;

# Make sure the episode number is padded with zeroes

$episode = sprintf "%04d", $episode;

$episode eq "0000" and die <<MSG;
FAILED!

Please supply an episode number like '0001' or '2'
Whatever floats your boat as long as it's a real episode

Really testing the input for all manner of totally whack 
non-numeric values is kind of a pain in the ass. Please just
pass a number, eh?

MSG


# Build some strings we'll need

my $baseUrl = 'http://www.freakangels.com';
my $imgPrefix = "$baseUrl/comics/FA$episode-";
my $imgSuffix = ".jpg";

my $dirPath = "FreakAngels_$episode";


# Make a temp dir
mkdir $dirPath;
my $cwd = getcwd;  # Save this path for later
chdir $dirPath;

# Download the images

# Let's hope the assumption of 6 pages per episode holds true
for my $imgNumber (1 .. 6) {
   system "wget $imgPrefix$imgNumber$imgSuffix";
}

# cd ..
chdir $cwd;

# Construct the .cbt file
system "tar cvf $dirPath.cbt $dirPath";

# Clean up the temp dir
system "rm -rf $dirPath";
