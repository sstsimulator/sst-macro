#!/bin/sh

ltool=`which glibtoolize`
if [ "$?" -ne "0" ]; then
  ltool="libtoolize"
fi
$ltool \
  && autoreconf --force --install

