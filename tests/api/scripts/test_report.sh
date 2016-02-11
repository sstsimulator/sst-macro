
echo "num args: $#"

if [ $# -ne 3 ] 
then
	echo "usage: test_report.sh [executable] [config file] [results dir]"
	exit 2
fi

echo "------ Start testsuite report ------"

passed=0
except=0
unknown=0

$1 -f $2 -p testsuite_testmode=0 -p testsuite_numtests=true > tmp.out
numtests=`head -n 1 tmp.out`

for ((i=0;i<=numtests;i++)); do
    $1 -f $2 -p testsuite_testmode=$i -p testsuite_print=true &> tmp.out
    testname=`grep 'name:' tmp.out | head -n 1 | cut -d : -f 2`
	success=`grep "No Errors" $3/$i.out`
	if [ "$success" != "" ]
	then
		echo "$testname($i): Pass"
		passed=$((passed+1))
	else
		exception=`grep "exception:" $3/$i.out`
		
		if [ "$exception"  != "" ]
		then
			echo "$testname($i): $exception"
			except=$((except+1))
		else
			echo "$testname($i): Some other error occurred"
			unknown=$((unknown+1))
		fi
	fi
done


echo "------ Summary --------"
echo "Num Passed: $passed"
echo "Num Exceptions Thrown: $except"
echo "Num Unknown Errors: $unknown"

rm tmp.out
