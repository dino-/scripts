package SongInfo::Song;

use SongInfo::MP3;
use SongInfo::Ogg;


# This is really a factory method to construct the proper subclass.
sub new($$)
{
	my ($pkg, $filePath) = @_;
	$filePath =~ /.*\.(.*)$/;
	$ext = $1;
	undef $obj;
	if(defined $ext)
	{
		SWITCH:
		{
			$ext =~ /mp3/ && do
			{ $obj = new SongInfo::MP3($filePath); last SWITCH; };
			$ext =~ /ogg/ && do
			{ $obj = new SongInfo::Ogg($filePath); last SWITCH; };
		}
	}

	return $obj;
}

sub close($)
{
	# Default implementation does nothing
}

1;  # Module initialization successful
