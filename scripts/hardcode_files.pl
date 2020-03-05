#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;
use File::Basename;
use bytes;
use Erlang::Parser;
use Data::Dumper;
use v5.25;

sub slurp
{
	my ($filename) = @_;
	local $/ = undef;
	open my $file, '<', $filename or die "can't open $filename: $!";
	return <$file>;
}

my ($RTLD, $OTP_DIR) = @ARGV;

my %FILES;

my @LOCAL_FILES=`cat preload_local_files`; chomp (@LOCAL_FILES);
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

$OTP_DIR .= '/lib/erlang';
my @LDD = `/usr/bin/ldd $OTP_DIR/$bindir/beam`;
foreach my $l (@LDD) {
	if ($l =~ m@ => (.*) \(@) {
		push @LOCAL_FILES, $1;
	}
}

$FILES{'beam'} = slurp ("$OTP_DIR/$bindir/beam");
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
	open my $file, '<', "$f" or die "can't open $f: $!";
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
	print "/", $file, "\0", pack('N', length($data)), $data;
}
