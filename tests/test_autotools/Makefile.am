noinst_LIBRARIES = libcompute.a
libcompute_a_SOURCES = compute.cc

ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = test_autotools
test_autotools_SOURCES = main.cc
test_autotools_LDADD = libcompute.a

CLEANFILES = tmp.compute.d tmp.main.d
