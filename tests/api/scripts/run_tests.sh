
echo "num args: $#"

if [ $# -ne 3 ] 
then
	echo "usage: run_tests.sh [executable] [config file] [results dir]"
	exit 2
fi

rm $3/*.out
NPROC=0
tmp="tmp.out"

$1 -f $2 -p testsuite_testmode=0 -p testsuite_numtests=true > "$tmp"
numtests=`head -n 1 $tmp`

for ((i=0;i<=numtests;i++)); do
	tmpname="$tmp$i"
	$1 -f $2 -p testsuite_testmode=$i -p testsuite_print=true &> "$tmpname"
    testname=`grep 'name:' "$tmpname" | head -n 1 | cut -d : -f 2`
    echo "running test: $testname"
    rm "$tmpname"
	$1 -f $2 -p testsuite_testmode=$i &> $3/$i.out &
	NPROC=$(($NPROC+1))
    if [ "$NPROC" -ge 10 ]; then
        wait
        NPROC=0
    fi
done

 if [ "$NPROC" -ge 0 ]; then
        wait
        NPROC=0
    fi
