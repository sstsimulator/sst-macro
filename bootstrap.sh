#!/bin/sh

git submodule init
git submodule update
(glibtoolize || libtoolize) \
  && (cd sprockit && (glibtoolize || libtoolize) ) \
  && (cd dumpi && (glibtoolize || libtoolize) ) \
  && (cd sumi && (glibtoolize || libtoolize) ) \
  && autoreconf --force --install

