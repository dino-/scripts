#! /usr/bin/perl -w

#--------------------------------------------------------------------------
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
$basename - Transform an XML document with an XSLT stylesheet

usage:
    $basename --xml XMLPATH --xsl XSLTPATH [--output OUTPATH]
    $basename --help

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

# Set the destination for output
if ($opts{output}) {
    $opts{output} = ">$opts{output}";
}
else {  # No dest given, use STDOUT
    $opts{output} = '>-';
}


# Construct parser and XSLT objects
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
