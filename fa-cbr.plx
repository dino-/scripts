#! /usr/bin/perl

# This script will take the URL to an episode of Warren Ellis'
# FreakAngels weekly online comic and bundle the pages into a
# Comic Book Archive file on your local system.

# Written by adoring fan Dino Morelli <dino@ui3.info>
# 2008-02-15

# This is, of course, free software. Give it to your friends. Enjoy

# Required for use: UNIX-like system with perl, wget, tar, rm

use strict;
use warnings;
use Cwd;


# Check the incoming URL argument
my $origUrl = shift;

$origUrl or die <<MSG;
FAILED!

Please supply a URL to an episode similar to this:
http://www.freakangels.com/2008/02/15/episode-0001/

MSG


# Build some strings we'll need

# The URL to the episode as a whole looks like this:
#   http://www.freakangels.com/2008/02/15/episode-0001/
#
# But we need the paths to the images to look like this:
#   http://www.freakangels.com/comics/FA0001-1.jpg
#
# So some string surgery:

my ($baseUrl, $episode) = $origUrl =~ m|(http://[^/]+).*episode-(\d+).*|;
my $imgPrefix = "$baseUrl/comics/FA$episode-";
my $imgSuffix = ".jpg";

my $dirPath = "FreakAngels_$episode";


# Make a temp dir
mkdir $dirPath;
my $cwd = getcwd;  # Save this path for later
chdir $dirPath;

# Download the images

my $imgNumber = 1;
$? = 0;
while ($? == 0) {
   system "wget $imgPrefix$imgNumber$imgSuffix";
   $imgNumber++;
}

# cd ..
chdir $cwd;

# Construct the .cbt file
system "tar cvf $dirPath.cbt $dirPath";

# Clean up the temp dir
system "rm -rf $dirPath";
