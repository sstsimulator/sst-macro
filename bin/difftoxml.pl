#!/usr/bin/env perl

# default parameters
$suitename = "diff";
$xmlout = "diff.test_report.xml";
$outfiledir = ".";
$chkfiledir = ".";
$outsuffix = "out";
$chksuffix = "chk";
$verbose = 0;
@results = ();
@failing_results = ();

while (@ARGV > 0) {
    $arg = shift;
    if ($arg eq "--xmlout") {
        $xmlout = shift;
    }
    elsif ($arg eq "--outfiledir") {
        $outfiledir = shift;
    }
    elsif ($arg eq "--chkfiledir") {
        $chkfiledir = shift;
    }
    elsif ($arg eq "--outsuffix") {
        $outsuffix = shift;
    }
    elsif ($arg eq "--chksuffix") {
        $chksuffix = shift;
    }
    elsif ($arg eq "--suitename") {
        $suitename = shift;
    }
    elsif ($arg eq "--verbose") {
        $verbose = 1;
    }
    else {
        $results[$#results+1] = $arg;
    }
}

$ntest = 0;
$nfail = 0;

for $result (@results) {
    $outfile = "$outfiledir/$result.$outsuffix";
    $chkfile = "$chkfiledir/$result.$chksuffix";
    open(DIFF,"diff -u $outfile $chkfile|");
    $difftext{$result} = "";
    while (<DIFF>) {
        $difftext{$result} = "$difftext{$result}$_";
    }
    $match{$result} = close(DIFF);
    $ntest = $ntest + 1;
    if (! $match{$result}) {
        $nfail = $nfail + 1;
        $failing_results[$#failing_results+1] = $result;
    }
}

open(XMLOUT, ">$xmlout");

print XMLOUT "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
print XMLOUT "<testsuite";
print XMLOUT " name=\"$suitename\"";
print XMLOUT " tests=\"$ntest\"";
print XMLOUT " failures=\"$nfail\"";
print XMLOUT ">\n";

for $result (@results) {
    $outfile = "$outfiledir/$result.$outsuffix";
    $chkfile = "$chkfiledir/$result.$chksuffix";
    if ($match{$result}) {
        print XMLOUT "  <testcase name=\"$result\" status=\"run\"/>\n";
    }
    else {
        $escaped_difftext = $difftext{$result};
        $escaped_difftext =~ s/]]>/]]>]]&gt;<![CDATA[/g;
        print XMLOUT "  <testcase name=\"$result\" status=\"run\">\n";
        print XMLOUT "    <failure message=\"files $outfile and $chkfile differ\">";
        print XMLOUT "<![CDATA[$escaped_difftext]]></failure>\n";
        print XMLOUT "  </testcase>\n";
        if ($verbose) {
            print "========================================================================\n";
            print "files $outfile and $chkfile differ:\n";
            print "------------------------------------------------------------------------\n";
            print "$difftext{$result}";
        }
    }
}

print XMLOUT "</testsuite>\n";

close(XMLOUT);

print "========================================================================\n";
print "\"$suitename\": ran $ntest tests and found $nfail failures\n";
print "------------------------------------------------------------------------\n";
if ($nfail > 0) {
    print "failing cases:\n";
    for $failing_result (@failing_results) {
        print "  $failing_result\n";
    }
print "========================================================================\n";
}

exit 1 if ($nfail > 0);
