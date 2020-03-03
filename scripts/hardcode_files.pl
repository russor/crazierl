#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;
use File::Basename;
use bytes;
use v5.25;

my $OTP_DIR = '../otp_src_R12B-5';

my %FILES;

my @OTP_FILES=`cat preload_otp_files`; chomp (@OTP_FILES);
my @LOCAL_FILES=`cat preload_local_files`; chomp (@LOCAL_FILES);
push @LOCAL_FILES, @ARGV;

foreach my $f (@OTP_FILES) {
	next unless $f;
	open my $file, '<', "$OTP_DIR/$f" or die "can't open $OTP_DIR/$f: $!";
	{
		local $/ = undef;
		$FILES{$f} = <$file>;
	}
	if ($f =~ m@/beam@) { # beam executable
		my @LDD = `/usr/bin/ldd $OTP_DIR/$f`;
		foreach my $l (@LDD) {
			if ($l =~ m@ => (.*) \(@) {
				push @LOCAL_FILES, $1;
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
