#! /usr/bin/env bash

find `pwd` -name "*.hpp" > sstmac_headers
if [ ! -d build ]; then 
  mkdir build
fi
cd build
../configure SST

