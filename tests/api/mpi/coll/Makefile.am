# -*- Mode: Makefile; -*-
# vim: set ft=automake :
#
# (C) 2011 by Argonne National Laboratory.
#     See COPYRIGHT in top-level directory.
#

include $(top_srcdir)/tests/mpi/Makefile.mtest

EXTRA_DIST = testlist

## for all programs that are just built from the single corresponding source
## file, we don't need per-target _SOURCES rules, automake will infer them
## correctly
noinst_PROGRAMS =      \
    allgather2         \
    allgather3         \
    allgatherv2        \
    allgatherv3        \
    allgatherv4        \
    allred             \
    allred2            \
    allred3            \
    allred4            \
    allred5            \
    allred6            \
    allredmany         \
    alltoall1          \
    alltoallv          \
    alltoallv0         \
    alltoallw1         \
    alltoallw2         \
    alltoallw_zeros    \
    bcast2             \
    bcast3             \
    bcasttest          \
    bcastzerotype      \
    coll2              \
    coll3              \
    coll4              \
    coll5              \
    coll6              \
    coll7              \
    coll8              \
    coll9              \
    coll10             \
    coll11             \
    coll12             \
    coll13             \
    exscan             \
    exscan2            \
    gather             \
    gather2            \
    iallred            \
    icallgather        \
    icallgatherv       \
    icallreduce        \
    icalltoall         \
    icalltoallv        \
    icalltoallw        \
    icbarrier          \
    icbcast            \
    icgather           \
    icgatherv          \
    icreduce           \
    icscatter          \
    icscatterv         \
    longuser           \
    nonblocking        \
    nonblocking2       \
    nonblocking3       \
    op_commutative     \
    opband             \
    opbor              \
    opbxor             \
    opland             \
    oplor              \
    oplxor             \
    opmax              \
    opmaxloc           \
    opmin              \
    opminloc           \
    opprod             \
    opsum              \
    red3               \
    red4               \
    red_scat_block     \
    red_scat_block2    \
    redscat            \
    redscat2           \
    redscat3           \
    redscatblk3        \
    redscatinter       \
    redscatbkinter     \
    reduce             \
    reduce_local       \
    scantst            \
    scatter2           \
    scatter3           \
    scattern           \
    scatterv           

allgatherv4_LDADD = $(LDADD) -lm

# This is here instead of Makefile.mtest because it is only used in this 
# set of tests
$(top_builddir)/util/nbc_pmpi_adapter.$(OBJEXT): $(top_srcdir)/tests/mpi/util/nbc_pmpi_adapter.c
	(cd $(top_builddir)/util && $(MAKE) nbc_pmpi_adapter.$(OBJEXT))
