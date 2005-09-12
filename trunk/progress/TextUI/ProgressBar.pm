package TextUI::ProgressBar;

#--------------------------------------------------------------------------
# $HeadURL: file:///var/lib/svn/scripts/trunk/progress/TextUI/ProgressBar.pm $
#--------------------------------------------------------------------------
# $Revision: 156 $
# $Date: 2005-09-12 14:00:39 -0400 (Mon, 12 Sep 2005) $
# $Author: dmorelli $
#
# Class to draw a text progress bar.
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Construct a new progress bar.
# args:
#   class
#   maxValue  max actual value
sub new {
	my $this = (bless {}, shift);
	$this->init(shift);

	return $this;
}

#--------------------------------------------------------------------------
# Initialize this instance
# args:
#   instance
#   maxValue  max actual value
sub init {
	my $this = shift;

	$this{actualMax} = shift;
	$this->setDisplayWidth(20);
	$this->setPercentFormat('%#3d');
}

#--------------------------------------------------------------------------
# Set the maximum width for the [==> ] portion of the progress bar.
sub setDisplayWidth {
	my $this = shift;
	$this{displayMax} = shift;
}

#--------------------------------------------------------------------------
# Set the printf format string for the percentage.
sub setPercentFormat {
	my $this = shift;
	$this{percentFormat} = shift;
}

#--------------------------------------------------------------------------
# Redraw the display.
# Pass in current so-far value.
sub update {
	my ($this, $value) = @_;

	my $displayCurr = ($value / $this{actualMax}) *
		$this{displayMax};

	# We need autoflush on but don't mess with the global value.
	local $| = 1;

	my $dcAsInt = int($displayCurr);
	print ("\r[" . ('=' x $dcAsInt) . ">" .
		(' ' x ($this{displayMax} - $dcAsInt)));

	printf("| $this{percentFormat}%%]", 
		($value / $this{actualMax}) * 100);

	# FIXME debugging
	#print "  $value  $displayCurr";
}

1;
