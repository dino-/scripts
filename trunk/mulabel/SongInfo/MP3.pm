package SongInfo::MP3;

use MP3::Tag;
use SongInfo::Song;


@ISA = qw(SongInfo::Song);

sub new($$)
{
	my ($pkg, $filePath) = @_;
	$this = (bless { }, $pkg);
	$this->open($filePath);

	return $this;
}

sub open($$)
{
	my ($this, $filePath) = @_;

	$this->{mMp3} = new MP3::Tag($filePath);
	$this->{mMp3}->get_tags;
}

sub getComment($$)
{
	my ($this, $key) = @_;
	my $id3v1 = $this->{mMp3}->{ID3v1};

	if(defined $id3v1) { return $id3v1->$key; }
	else { return; }
}

sub close($)
{
	my $this = shift;
	my $mp3 = $this->{mMp3};

	if(defined $mp3) { $mp3->close(); }
}

1;  # Module initialization successful
