#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;
use File::Basename;
use bytes;
use Data::Dumper;
use File::Temp;
#use Compress::LZ4;
use v5.25;

sub slurp
{
	my ($filename) = @_;
	local $/ = undef;
	open my $file, '<', $filename or die "can't open $filename: $!";
	my $data = <$file>;
	if (substr($data, 0, 4) eq "\x7FELF") {
		my $tmp = File::Temp->new();
		if (system("strip $filename -o $tmp") == 0) {
			my $stripped = <$tmp>;
			if (length($stripped) < length($data)) {
				print STDERR "using stripped instead of raw for $filename ", length($stripped), " < ", length($data), "\n";
				return $stripped;
			}
		}
	}
	return $data;
}

my ($TARGET, $RTLD, $OTP_DIR, @LOCAL_FILES) = @ARGV;

my %FILES;

push @LOCAL_FILES, $RTLD;

open my $erl, '<', "$OTP_DIR/bin/erl" or die "can't open erl: $!";

my $bindir;
while (<$erl>) {
	if (m@BINDIR="\$ROOTDIR/(.*)"$@) {
		$bindir = $1;
		last;
	}
}
if (!$bindir) {
	die "couldn't find BINDIR\n";
}

push @LOCAL_FILES, "OTPDIR/$bindir/beam.smp";

$FILES{'bin/start.boot'} = slurp("$OTP_DIR/bin/start.boot");

my @start_script = `$OTP_DIR/bin/escript ./extract_start.escript $OTP_DIR/bin/start.script`;
die "couldn't run extract_start.escript" unless $? == 0;

chomp (@start_script);
my @path = ();
my %basenames = ($TARGET => "target");
foreach my $line (@start_script) {
	my ($type, @rest) = split /\t/, $line;
	if ($type eq 'path') {
		@path = ();
		foreach my $p (@rest) {
			$p =~ s@^\$ROOT/@@;
			push @path, $p;
		}
	} elsif ($type eq 'primLoad') {
		foreach my $m (@rest) {
			my $found = 0;
			foreach my $p (@path) {
				my $path_name = "$p/$m.beam";
				my $full_name = "$OTP_DIR/$path_name";
				if (-r $full_name) {
					$FILES{$path_name} = slurp ($full_name);
					$basenames{basename($path_name)} = $path_name;
					$found = 1;
				}
			}
			if (!$found) {
				die "couldn't find $m.beam";
			}
		}
	}
}

foreach my $f (@LOCAL_FILES) {
	next unless $f;
	my $path = $f;
	if ($f =~ m@^OTPDIR/(.*)$@) {
		$f = $1;
		($f) = glob ("$OTP_DIR/$f");
		if (!$f) {
			die "couldn't glob $path $OTP_DIR";
		}
		if (substr($f, 0, length($OTP_DIR) + 1) eq "$OTP_DIR/") {
			substr($f, 0, length($OTP_DIR) + 1, '');
		} else {
			die "couldn't find $f";
		}
		$path = "$OTP_DIR/$f";
	}
	my $file;
	open $file, '<', "$path" or die "can't open $path: $!";
	$f =~ s@^/@@;
	my $alt = $basenames{basename($f)};
	if ($alt) {
		$f = $alt;
	}
	{
		local $/ = undef;
		$FILES{$f} = <$file>;
	}
	if ($f eq 'target' || $f =~ /\.so$/) {
		my @LDD = `/usr/bin/ldd $path`;
		foreach my $l (@LDD) {
			if ($l =~ m@ => (.*) \(@) {
				push @LOCAL_FILES, $1;
			}
		}
	}
}

my $strlen_total = 0;

foreach my $file (keys %FILES) {
	$strlen_total += length($file) + 1;
}

print pack('N', scalar(%FILES)), pack('N', $strlen_total);

foreach my $file (sort keys %FILES) {
	my $data = $FILES{$file};
#	my $compressed_data = lz4_compress_hc($data, 16);
#	if (length($compressed_data) + 4 < length($data)) {
#		print "/", $file, "\0", pack('NN', length($data) | 0x80000000, length($compressed_data)), $compressed_data;
#	} else {
		print "/", $file, "\0", pack('N', length($data)), $data;
#	}
}
