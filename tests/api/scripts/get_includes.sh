

for i in *.cc
do
    echo "#include \"${PWD##*/}/$i\""
done