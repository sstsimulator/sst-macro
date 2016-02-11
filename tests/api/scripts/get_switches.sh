

dirs=(attr coll comm datatype errhan group info init pt2pt topo)
len=${#dirs[*]}

echo "num of dirs: $len"
index=0
for (( i=0; i<len; i++ )) 
do

for file in ${dirs[$i]}/*.cc
do
	basename=`echo $file|sed 's/\.cc$//g'`
	nodir=`echo $basename|sed 's!.*/!!' `
	upper=`echo "${dirs[$i]}_$nodir"|tr "[:lower:]" "[:upper:]"`
	enumname=`echo $upper|sed 's/-/_/g'`
   
    echo "case $enumname: $nodir::$nodir(argc, argv); break;"
done

done