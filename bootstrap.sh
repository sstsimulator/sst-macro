#!/bin/sh

(glibtoolize || libtoolize) \
  && (cd sprockit && (glibtoolize || libtoolize) ) \
  && (cd dumpi && (glibtoolize || libtoolize) ) \
  && (cd pth && (glibtoolize || libtoolize) ) \
  && (cd dharma && (glibtoolize || libtoolize) ) \
  && autoreconf --force --install

