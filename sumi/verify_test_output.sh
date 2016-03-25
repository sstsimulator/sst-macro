#!/bin/bash

reffile=$1
shift
cmdline=$*

$cmdline | diff --ignore-matching-lines="SST\/macro\sran\sfor" - $reffile
