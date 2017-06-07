#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

# Files that should be distributed but are not built.
EXTRA_DIST = LICENSE bootstrap.sh \
  developer-sstmacro-7.1.pdf \
  manual-sstmacro-7.1.pdf \
  pdes-report-7.1.pdf \
  docs \
  skeletons

SUBDIRS = $(subdirs) sumi-mpi

SUBDIRS += sstmac bin python tests configurations 

dist_bin_SCRIPTS = sstmacro-libtool

ACLOCAL_AMFLAGS = -I acinclude

QMAKE=@qmake_executable@

AM_FFLAGS = $(FFLAG_INT)
AM_CPPFLAGS += $(SCALAPACK_CPPFLAGS)
AM_CPPFLAGS += $(LAPACK_CPPFLAGS)
AM_CPPFLAGS += $(BLAS_CPPFLAGS)

AM_LDFLAGS += $(SCALAPACK_LDFLAGS)
AM_LDFLAGS += $(LAPACK_LDFLAGS)
AM_LDFLAGS += $(BLAS_LDFLAGS)

if EXTERNAL_BOOST
AM_LDFLAGS += $(BOOST_LDFLAGS)
endif

.PHONY: doc superclean gui
doc:
	cd docs && doxygen doxygen.cfg
	cd sst-dumpi && make doc

#doc-install: doc
#	$(INSTALL) -d docs/sst-macroscale @docdir@
#	cd dumpi && make doc-install
#

if REPO_BUILD
sstmac_repo.h: .git/refs/heads/@git_branch@
	cd $(abs_top_srcdir) && $(abs_top_srcdir)/bin/make_repo_header $(abs_top_srcdir) sstmac
endif

sstmacro-libtool: libtool
	cp -fp $< $@

superclean: clean distclean-am
	rm -rf configure  autom4te.cache bin aclocal.m4 src/sstmacconfig.h.in config.log config.status
	rm -f acinclude/ltsugar.m4 acinclude/libtool.m4 acinclude/ltversion.m4 acinclude/lt~obsolete.m4 acinclude/ltoptions.m4
	rm -f src/sstmacconfig.h src/stamp-h1
	cd sst-dumpi; $(MAKE) superclean
	find . -name Makefile.in -type f |xargs rm -f
	find . -name .deps -type d |xargs rm -rf
	find . -name Makefile -type f |xargs rm -f
	rm -f tests/stats.log

clean-local:
	rm -f sstmacro-libtool
	rm -f _configs.sed
	rm -rf $(XCODEBUILD_TMPDIR)


