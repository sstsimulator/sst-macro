EXTRA_DIST=ini ref

if !INTEGRATED_SST_CORE
check_PROGRAMS = \
  sst_box_domain \
  sst_barrier \
  sst_deadlock \
  sst_big_tree \
  sst_replica \
  sst_collective \
  sst_heartbeat \
  sst_failure \
  sst_domain \
  sst_failure_lazy \
  sst_failure_nack

#  sst_saturation 

AM_CPPFLAGS = -I$(top_builddir) -I$(top_srcdir) 
AM_CPPFLAGS += -I$(top_srcdir)/sumi -I$(top_builddir)/sumi
AM_CPPFLAGS += -I$(top_srcdir)/sprockit -I$(top_builddir)/sprockit
AM_CPPFLAGS += -I$(top_srcdir)/sstmac/install -DSSTMAC=1
AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements -I$(top_srcdir)/sstmac/replacements 
AM_CPPFLAGS += -Dmain=USER_MAIN -DSSTMAC=1

exe_LDADD = ../../sstmac/install/libsstmac.la ../../sprockit/sprockit/libsprockit.la ../../sstmac/main/libsstmac_main.la

sst_deadlock_SOURCES = deadlock.cc
sst_big_tree_SOURCES = big_tree.cc
sst_replica_SOURCES = replica.cc
#sst_saturation_SOURCES = saturation.cc
sst_collective_SOURCES = collective.cc
sst_heartbeat_SOURCES = heartbeat.cc
sst_failure_SOURCES = failure.cc
sst_domain_SOURCES = domain.cc
sst_box_domain_SOURCES = box_domain.cc
sst_failure_lazy_SOURCES = failure.cc
sst_failure_nack_SOURCES = failure_nack.cc
sst_barrier_SOURCES = barrier.cc


sst_barrier_LDADD = $(exe_LDADD)
sst_deadlock_LDADD = $(exe_LDADD)
sst_big_tree_LDADD = $(exe_LDADD)
sst_replica_LDADD = $(exe_LDADD)
#sst_saturation_LDADD = $(exe_LDADD)
sst_collective_LDADD = $(exe_LDADD)
sst_heartbeat_LDADD = $(exe_LDADD)
sst_failure_LDADD = $(exe_LDADD)
sst_domain_LDADD = $(exe_LDADD)
sst_box_domain_LDADD = $(exe_LDADD)
sst_failure_lazy_LDADD = $(exe_LDADD)
sst_failure_nack_LDADD = $(exe_LDADD)


endif
