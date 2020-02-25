#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;
use File::Basename;

my @FILES=`cat hardcoded_files`;
chomp @FILES;

my $OUTPUTDIR=cwd() . '/fs_obj';
foreach (glob ("$OUTPUTDIR/*.o")) {
	unlink($_) || die "couldn't unlink $_";
}

open my $out, '>', 'files.c' or die "couldn't open output file";

chdir "../otp_src_R12B-5" || die "couldn't chdir";

foreach my $f (@FILES) {
	die unless $f;
	my $base = basename($f);
	system("objcopy -I binary -O elf32-i386-freebsd $f $OUTPUTDIR/$base.o") == 0 || die "couldn't objcopy $f";
}

print $out <<'EOS';
// automatically generated by hardcode_files.pl
#include "files.h"
#include <string.h>

EOS
my @TOKENS;
foreach my $f (@FILES) {
	my $token = $f;
	$token =~ s@[\./]@_@g;
	push @TOKENS, $token;
	print $out "extern uint32_t _binary_${token}_start;\n";
	print $out "extern uint32_t _binary_${token}_end;\n";
	print $out "extern uint32_t _binary_${token}_size;\n";
}

print $out "struct hardcoded_file hardcoded_files[" . scalar(@FILES) ."];\n";
print $out "void init_files() {\n";

for (my $i = 0; $i < @FILES; ++$i) {
	my $f = $FILES[$i];
	my $token = $TOKENS[$i];
	print $out <<EOS
	hardcoded_files[$i].name = \"/${f}\";
	hardcoded_files[$i].start = &_binary_${token}_start;
	hardcoded_files[$i].end = &_binary_${token}_end;
	hardcoded_files[$i].size = &_binary_${token}_size;
EOS
}
print $out <<'EOS';
};

struct hardcoded_file * find_file(const char * name) {
	for (int i = 0; i < sizeof(hardcoded_files); ++i) {
		if (strcmp(name, hardcoded_files[i].name) == 0) {
			return &(hardcoded_files[i]);
		}
	} 
	return NULL;
}
EOS