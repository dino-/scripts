#! /usr/local/bin/perl

#--------------------------------------------------------------------------
# $RCSfile$
#--------------------------------------------------------------------------
# $Revision: 4 $
# $Date: 2004-02-25 13:19:39 -0500 (Wed, 25 Feb 2004) $
# $Author: dmorelli $
#
# Script to turn an rgb.txt file (from *nix) into an html table showing
# The color names, values and swatches.
#--------------------------------------------------------------------------


main();

#--------------------------------------------------------------------------
sub constructHeader
{
	my $strOrigFile = $_[0];

	$strHeader = 
		"<html>\n\t<head>\n\t\t<title>rgb.txt listing</title>\n\t";
	$strHeader .= "</head>\n\n\t<body>\n";
	$strHeader .= "\t\t<p />Constructed from ${strOrigFile}\n";
	$strHeader .= "\t\t<br />Running on: " . 
		qx/uname -a/ . "\n";
	$strHeader .= "\t\t<br />Constructed on " . localtime() . "\n";
	$strHeader .= "\t\t<p /><table border>\n";
	$strHeader .= "\t\t<tr><td><b>color name</b></td>";
	$strHeader .= "<td><b>swatch</b></td>";
	$strHeader .= "<td><b>#rrggbb (hex)</b></td>";
	$strHeader .= "<td><b>R G B (dec)</b></td>";
	$strHeader .= "</tr>\n";

	return $strHeader;
}  # sub constructHeader

#--------------------------------------------------------------------------
# Take the decimal RGB values and color name and construct a table row.
sub constructTableRow
{
	# These three assingments perform a dec to hex conversion.
	$nR = sprintf("%02x", $_[0]);
	$nG = sprintf("%02x", $_[1]);
	$nB = sprintf("%02x", $_[2]);
	$strColorDec = "$_[0] $_[1] $_[2]";
	$strColorHex = "#${nR}${nG}${nB}";

	$strName = $_[3];

	$strRow = "\t\t\t<tr><td>${strName}</td>";
	$strRow .= "<td width='200' bgcolor='${strColorHex}'>";
	$strRow .= "&nbsp;</td>";
	$strRow .= "<td>${strColorHex}</td>";
	$strRow .= "<td>${strColorDec}</td>";
	$strRow .= "</tr>\n";

	return $strRow;
}  # sub constructTableRow

#--------------------------------------------------------------------------
sub constructFooter
{
	return "\t\t</table>\n\t</body>\n</html>";
}  # sub constructFooter

#--------------------------------------------------------------------------
sub main
{
	my ($strFileIn, $strFileOut) = @ARGV;

	$strHtml = constructHeader($strFileIn);

	open(fileRgb, $strFileIn)
		|| die("Could not open file: $strFilePath\n");

	# Skip the first line, it's always a comment.
	$strLine = <fileRgb>;

	# Walk the remaining lines and extract the info we want.
	while($strLine = <fileRgb>)
	{
		$strLine =~ m/[ \t]*([0-9]{1,3})[ \t]*([0-9]{1,3})[ \t]*([0-9]{1,3})[ \t]*(.*)/;
		$strHtml .= constructTableRow($1, $2, $3, $4);
	}

	close(fileRgb);

	$strHtml .= constructFooter();

	# FIXME
	# Instead of printing, how about we save the file.
	#print("$strHtml\n");

	open(fileOut, ">" . $strFileOut);
	print(fileOut "$strHtml\n");
	close(fileOut);
}  # sub main

