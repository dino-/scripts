#! /usr/bin/perl -w

#--------------------------------------------------------------------------
# $HeadURL: file:///var/lib/svn/scripts/trunk/rgbtable/rgbtable.pl $
#--------------------------------------------------------------------------
# $Revision: 156 $
# $Date: 2005-09-12 14:00:39 -0400 (Mon, 12 Sep 2005) $
# $Author: dmorelli $
#
# Script to turn an rgb.txt file (from *nix) into an html table showing
# The color names, values and swatches.
#--------------------------------------------------------------------------

use HTML::Template;


main();

#--------------------------------------------------------------------------
# Take the decimal RGB values and color name and construct a table row.
sub constructTableRow($$$$)
{
	my %rowData;  # Get a fresh hash for the row data

	# These three assingments perform a dec to hex conversion.
	$r = sprintf("%02x", $_[0]);
	$g = sprintf("%02x", $_[1]);
	$b = sprintf("%02x", $_[2]);
	$colorDec = "$_[0] $_[1] $_[2]";
	$colorHex = "#${r}${g}${b}";

	$rowData{colorName} = $_[3];
	$rowData{colorHex} = $colorHex;
	$rowData{colorDec} = $colorDec;

	# Return a reference to the hash.
	return { %rowData };
}  # sub constructTableRow

#--------------------------------------------------------------------------
sub main
{
	my $pathIn = shift @ARGV;

	# open the html template
	my $template = HTML::Template->new(filename => 'rgbtable.tmpl');

	$template->param(fileOrig => $pathIn);
	$template->param(platform => qx/uname -a/);
	$template->param(dateTime => scalar localtime());

	open(fileRgb, $pathIn)
		|| die("Could not open file: $pathIn\n");

	# initialize an array to hold your loop
	my @loopData = ();

	# Skip the first line, it's always a comment.
	$line = <fileRgb>;

	# Walk the remaining lines and extract the info we want.
	while($line = <fileRgb>)
	{
		$line =~ m/[ \t]*([0-9]{1,3})[ \t]*([0-9]{1,3})[ \t]*([0-9]{1,3})[ \t]*(.*)/;

		# Push a reference to this row into the loop
		push(@loopData, constructTableRow($1, $2, $3, $4));
	}

	close(fileRgb);

	$template->param(colors => \@loopData);

	# Output the result document.
	print $template->output;
}  # sub main

