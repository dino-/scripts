package SongInfo::Ogg;

use Ogg::Vorbis::Header::PurePerl;
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

	$this->{mOgg} = new Ogg::Vorbis::Header::PurePerl($filePath);
}

sub getComment($$)
{
	my ($this, $key) = @_;
	return ($this->{mOgg}->comment($key))[0];
}
