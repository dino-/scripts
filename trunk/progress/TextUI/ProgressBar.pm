package TextUI::ProgressBar;

#--------------------------------------------------------------------------
# $RCSfile$
#--------------------------------------------------------------------------
# $Revision: 36 $
# $Date: 2004-07-08 12:00:50 -0400 (Thu, 08 Jul 2004) $
# $Author: dmorelli $
#
# Class to draw a text progress bar.
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Construct a new progress bar.
# Pass in the max value you will be counting up to.
sub new($$)
{
	my $this = (bless {}, shift);
	$this{actualMax} = shift;
	$this{actualCurr} = 0;
	$this{displayMax} = 20;
	$this{percentFormat} = "%#3d";

	return $this;
}

#--------------------------------------------------------------------------
# Set the maximum width for the [==> ] portion of the progress bar.
sub setDisplayWidth($$)
{
	my $this = shift;
	$this{displayMax} = shift;
}

#--------------------------------------------------------------------------
# Set the printf format string for the percentage.
sub setPercentFormat($$)
{
	my $this = shift;
	$this{percentFormat} = shift;
}

#--------------------------------------------------------------------------
# Redraw the display.
# Pass in current so-far value.
sub update($$)
{
	my ($this, $value) = @_;

	$this{actualCurr} = $value;
	my $displayCurr = ($this{actualCurr} / $this{actualMax}) *
		$this{displayMax};

	# We need autoflush on but don't mess with the global value.
	local $| = 1;

	print "\r[";
	my $i = 0;
	while($i < $displayCurr - 1)
	{
		print "=";
		$i++;
	}
	print ">";
	$i++;
	while($i < $this{displayMax})
	{
		print " ";
		$i++;
	}

	my $percent = ($this{actualCurr} / $this{actualMax}) * 100;

	printf("| $this{percentFormat}%%]", $percent);

	# FIXME debugging
	#print "  $this{actualCurr}  $displayCurr";
}

1;  # Module initialized successfully.
