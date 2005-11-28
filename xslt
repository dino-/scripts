#! /usr/bin/perl -w

#--------------------------------------------------------------------------
# $HeadURL: file:///var/lib/svn/scripts/xslt $
#--------------------------------------------------------------------------
# $Revision: 166 $
# $Date: 2005-11-28 10:55:05 -0500 (Mon, 28 Nov 2005) $
# $Author: dmorelli $
#
# Transform an XML document with an XSLT stylesheet
#--------------------------------------------------------------------------

use strict;
use File::Basename;
use FileHandle;
use Getopt::Long;
use XML::LibXML;
use XML::LibXSLT;


my $basename = basename $0;
my $usage = <<USAGE;
$0 - Transform an XML document with an XSLT stylesheet

usage:
    $0 --xml XMLPATH --xsl XSLTPATH [--output OUTPATH]
    $0 --help

options:
    -o, --output=OUTPATH   Path of file in which to save output
                           Default: STDOUT
    -s, --xsl=XSLTPATH     Path to XSLT stylesheet
    -x, --xml=XMLPATH      Path to XML document to be transformed
    -h, --help             This help information
USAGE


# Parse the args
my %opts;
Getopt::Long::Configure("bundling");
GetOptions (\%opts,
    'xml|x=s',
    'xsl|s=s',
    'output|o=s',
    'help|h',
) or die "$usage\n";

die "$usage\n" if ($opts{help});

die "Missing args\n$usage\n" if (!$opts{xml} || !$opts{xsl});

if ($opts{output}) {
    $opts{output} = ">$opts{output}";
} else {
    $opts{output} = '>-';
}


my $parser = XML::LibXML->new();
my $xslt = XML::LibXSLT->new();

# source is the XML document to be transformed
my $source = $parser->parse_file($opts{xml});

# style_doc is the XSLT stylesheet
my $style_doc = $parser->parse_file($opts{xsl});
my $stylesheet = $xslt->parse_stylesheet($style_doc);

# results is the resulting XML::LibXML::Document object
my $results = $stylesheet->transform($source);

# Output the result somewhere
my $fh = FileHandle->new($opts{output});
$stylesheet->output_fh($results, $fh);
