#!/usr/bin/perl
# --------------------------------------------------------------------
# 					*** bkx ***
#
#	Extract and process data from a B&K data logger output file
#					(A Perl Script)
#
#				by Neil E. Klepeis
#
#  Note:  run 'perldoc' on this file to obtain the pod manual,
#  		i.e., pod tags and embedded documentation are included
# 		in the Perl script.
#
#  For example:
#
#  ===> perldoc foo
#
#  An html version of the manual can be obtained using the
#  'pod2html' translator.
#
#  ===> pod2html --infile=foo --outfile=foo.html
# --------------------------------------------------------------------

=head1 NAME

bkx - extract and process data from B & K data logger output file

=head1 VERSION

Version 0.0.1, 27 Sep 2000

=head1 SYNOPSIS

 bkx -i inputfile [-f <field number to extract>] [-t <tag for new data file>]

=head1 GENERAL DESCRIPTION

[Based on the BASIC program F<bkfix.bas> by Wayne R. Ott]

A Perl script to extract a certain column of a B & K data logger
output file and create a file containing the appropriate data values
and the corresponding elapsed number of minutes.  The user can specify
the data field (column) of data to extract.  The default is data field number
1 (the first data field after the date).   The user also specifies a TAG
to place at the beginning of each outputted record.  Output is written to STDOUT.
The output contains a field for the minutes after midnight (MAM) and the elapsed
minutes (ELM) from the time at the beginning of the file.

The -h switch turns on the writing of a header to the output.

(Tested with Perl 5.005)

I<Please Note>:  The comments in this manual page are extracted from the
actual Perl script file F<foo.pl>.  Any reference to code in the
text refers to this script file.

The original file was created by Neil Klepeis on 4 Aug 1999.
Last Update:  27 Sep 2000 nk

=head1 DISTRIBUTION LICENSE

Copyright (C) 2000  Neil E. Klepeis

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License (GPL)
as published by the Free Software Foundation (FSF); either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program (see F<GNU_GPL.txt> file); if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

=head1 AUTHOR CONTACT INFORMATION

 Neil E. Klepeis
 Indoor Environment Department
 Energy and Environmental Technologies Division
 Lawrence Berkeley National Laboratory (LBNL)
 One Cyclotron Road, Mailstop 90-3058
 Berkeley, CA 94720

 Email: mailto:nklepeis@uclink4.berkeley.edu
 Web:  http://eetd.lbl.gov/ied/era/exposuremodeling

=cut


use strict;
use FileHandle;
use Getopt::Std;

my %option = ();
getopt("ift", \%option);
getopts("h", \%option);

my ($tag) = ($option{t});
my ($field) = ($option{f});
my ($infile) = $option{i};

my ($i, $j, $extra, $cell, $numcols, @record);

print STDERR "\nEXTRACTING FROM B & K OUTPUT FILE: \"$infile\"\n";

if (!defined($infile)) {print STDERR "\aEXITING: MUST SPECIFY INPUT FILE WITH -i OPTION\n"; exit;};
if (!defined($field)) {$field = 1};
if (!defined($tag)) {$tag = $infile};

print STDERR "=" x 80;

my $inhand = new FileHandle ($infile) or die "Can't open file <$infile> for extraction!\n";   # open for read only

my ($j, @record, $day, $daycount, @time, $hours, $mins, $secs, $oldtime,
   $mins_after_midnight, $firstMAM, $elapsed_mins, $prevMAM, $value);

$prevMAM = 0;
$daycount = 0;

# print a header
if ($option{h}) {print STDOUT "#TAG\tDAY\tTIME\tMAM\tELM\tVALUE\n"}

LINE: while(<$inhand>) {        #goes one line at a time through the input file (old file)

		chomp;  #remove end of line character

		if (/^#/ || !/./) {print STDOUT $_;  next LINE;}  # pass all comment lines unprocessed
		if (!/./) {next LINE;}	# skip blank lines
		# skip lines containing text;
		#     passes numbers, non-alphs except e and E for sci notation
		if (/[a-df-zA-DF-Z]/) {next LINE;}

		$j++;
		@record = split();
		$value = $record[$field+1];
		$day = $record[0];
		@time = split /:/, $record[1];
		$hours = $time[0];
		$mins = $time[1];
		$secs = $time[2];

		$mins_after_midnight = sprintf("%.2f", 60*$hours + $mins + $secs/60);
		if ($j == 1) {$firstMAM = $mins_after_midnight}

		# should work now, though needs to be tested

		if ($mins_after_midnight <= $prevMAM) {$daycount++}

          $elapsed_mins = sprintf("%.2f", $mins_after_midnight - $firstMAM + $daycount*1440);

		$prevMAM = $mins_after_midnight;

		print STDOUT "$tag\t$day\t$hours:$mins:$secs\t$mins_after_midnight\t$elapsed_mins\t$value\n";

	}

$inhand->close;


__END__

