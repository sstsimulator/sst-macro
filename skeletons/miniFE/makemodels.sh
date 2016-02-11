#!/bin/bash

EIGER=~/eiger/eiger/Eiger.py

dc[1]=perform_element_loop
dc[2]=dirichlet
dc[3]=cgsolve

# number of pcs to use for each dc. from earlier runs.
pcs[1]=4
pcs[2]=5
pcs[3]=4

for x in 1 2 3
do
    $EIGER --db minife --passwd root --user root \
        --training-datacollection ${dc[x]} \
        --performance-metric ${dc[x]}_wtime \
        --show-prediction-statistics --test-fit \
        --plot-performance-scatter \
        --threshold 0.001 --pca-components ${pcs[x]} \
        --output ${dc[x]}.model > ${dc[x]}.out
done
