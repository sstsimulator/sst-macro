#!/bin/sh

git submodule init
git submodule update
ltool=`which glibtoolize`
if [ "$?" -ne "0" ]; then
  ltool="libtoolize"
fi
$ltool \
  && (cd sprockit && $ltool ) \
  && (cd sst-dumpi && $ltool ) \
  && (cd sumi && $ltool ) \
  && autoreconf --force --install

