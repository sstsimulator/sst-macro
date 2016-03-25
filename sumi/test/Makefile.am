
AM_CPPFLAGS = -I$(top_builddir) -I$(top_srcdir) 

bin_PROGRAMS = \
  pairwise \
  collective \
  failure \
  thread_safe_classes \
  thread_safe_refcount

exe_LDADD = ../sumi/libsumi.la

pairwise_SOURCES = pairwise.cc
failure_SOURCES = failure.cc
collective_SOURCES = collective.cc
thread_safe_classes_SOURCES = thread_safe_classes.cc
thread_safe_refcount_SOURCES = thread_safe_refcount.cc

pairwise_LDADD = $(exe_LDADD)
collective_LDADD = $(exe_LDADD)
failure_LDADD = $(exe_LDADD)
thread_safe_classes_LDADD = $(exe_LDADD)
thread_safe_refcount_LDADD = $(exe_LDADD)

