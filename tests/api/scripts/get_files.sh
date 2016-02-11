

dirs=(attr coll comm datatype errhan group info init pt2pt topo)
len=${#dirs[*]}

echo "num of dirs: $len"
index=0
for (( i=0; i<len; i++ )) 
do

for file in ${dirs[$i]}/*.cc
do
	echo "$file \\"
done

done