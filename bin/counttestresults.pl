#!/usr/bin/env perl

$tests = 0;
$failures = 0;
$disabled = 0;
$errors = 0;
while (<>) {
    # sample line:
    # <testsuite name="testpingpong" tests="3" failures="0" disabled="0" errors="0" time="0.285">
    if (/<\s*testsuite\s+/) {
        if (/\s+tests\s*=\s*"([0-9]+)"/) {
            $tests += $1;
        }
        if (/\s+failures\s*=\s*"([0-9]+)"/) {
            $failures += $1;
        }
        if (/\s+disabled\s*=\s*"([0-9]+)"/) {
            $disabled += $1;
        }
        if (/\s+errors\s*=\s*"([0-9]+)"/) {
            $errors += $1;
        }
    }
}

print "========================================================================\n";
print "RESULT SUMMARY: tests=$tests failures=$failures errors=$errors disabled=$disabled\n";
print "========================================================================\n";

exit 1 if ($failures + $errors + $disabled > 0);

