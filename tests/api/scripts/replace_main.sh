

for i in *.cc
do
  newname=`echo $i|sed 's/\.cc$//g'`
#  echo "$newname"
  sed -i "" "s/USER_MAIN/$newname/g" $i
#echo "$i"c
done