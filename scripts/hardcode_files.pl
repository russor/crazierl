#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;
use File::Basename;
use bytes;
use Erlang::Parser;
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

my ($RTLD, $OTP_DIR, @LOCAL_FILES) = @ARGV;

my %FILES;

push @LOCAL_FILES, $RTLD;

open my $erl, '<', "$OTP_DIR/bin/erl" or die "can't open erl: $!";

my $bindir;
while (<$erl>) {
	if (m@BINDIR=\$ROOTDIR/(.*)$@) {
		$bindir = $1;
		last;
	}
}
if (!$bindir) {
	die "couldn't find BINDIR\n";
}

#$OTP_DIR .= '/lib/erlang';
my $BEAM = "$OTP_DIR/$bindir/beam.smp";
my @LDD = `/usr/bin/ldd $BEAM`;
foreach my $l (@LDD) {
	if ($l =~ m@ => (.*) \(@) {
		push @LOCAL_FILES, $1;
	}
}

$FILES{'beam'} = slurp ("$BEAM");
$FILES{'bin/start.boot'} = slurp("$OTP_DIR/bin/start.boot");

my $start_script = "myfun() -> \n";
$start_script .= slurp ("$OTP_DIR/bin/start.script");
my @nodes = Erlang::Parser->parse($start_script);
my $statement = $nodes[0]->defs()->[0]->stmts()->[0] or die "can't parse start.script";

my @path = ();
foreach my $element (@{$statement->elems()->[2]->elems()}) {
	my ($key, $val) = @{$element->elems()};
	if ($key->atom eq 'path') {
		@path = ();
		foreach my $p (@{$val->elems()}) {
			$p = $p->string;
			$p =~ s@^\$ROOT/@@;
			push @path, $p;
		}
	} elsif ($key->atom eq 'primLoad') {
		foreach my $m (@{$val->elems()}) {
			$m = $m->atom;
			my $found = 0;
			foreach my $p (@path) {
				my $path_name = "$p/$m.beam";
				my $full_name = "$OTP_DIR/$path_name";
				if (-r $full_name) {
					$FILES{$path_name} = slurp ($full_name);
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
	my $file;
	if ($f =~ m@^OTPDIR/(.*)$@) {
		$f = $1;
		$f = glob ("$OTP_DIR/$f");
		if (substr($f, 0, length($OTP_DIR) + 1) eq "$OTP_DIR/") {
			substr($f, 0, length($OTP_DIR) + 1, '');
		} else {
			die "couldn't find $f";
		}
		open $file, '<', "$OTP_DIR/$f" or die "can't open $OTP_DIR/$f: $!";
	} else {
		open $file, '<', "$f" or die "can't open $f: $!";
	}
	$f =~ s@^/@@;
	{
		local $/ = undef;
		$FILES{$f} = <$file>;
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
