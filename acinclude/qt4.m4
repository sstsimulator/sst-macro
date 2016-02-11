

AC_DEFUN([CHECK_QT4], [

  
  dnl Look for the header
  AC_ARG_WITH(qt4_incl, AC_HELP_STRING([--with-qt4-incl=DIR],
             [directory where QT 4.x headers are]),
	     with_qt4_incl=${withval})

  AC_CACHE_VAL(ac_cv_path_qt4_incl,[
    if test x"${with_qt4_incl}" != x; then
      if test -f ${with_qt4_incl}/QtCore/qobject.h; then
        ac_cv_path_qt4_incl="-I`(cd ${with_qt4_incl}; pwd)` -I${with_qt4_incl}/QtCore -I${with_qt4_incl}/QtGui"
      else
        AC_MSG_WARN([${with_qt4_incl}/QtCore directory doesn't contain any QT 4.x headers])
      fi
    fi
  ])				dnl end of cache ac_cv_path_qt4_incl

  dnl Only run these tests if this version was specified by the user.


    if test x"${ac_cv_path_qt4_incl}" = x; then
      dnl if QTDIR is set in the users environment, use that, as that's what
      dnl most QT programmers do, as it's required by the QT build system.
      if test x$QTDIR != x; then
        if test -f $QTDIR/include/QtCore/qobject.h; then
          qt_base=`basename $QTDIR`
          dnl Only support version 4 in this file.
          if test x"${qt_base}" != x"qt4"; then
            AC_MSG_WARN([QT 4.x is required])
          else
          	ac_cv_path_qt4_incl="-I$QTDIR/include -I$QTDIR/include/QtCore -I$$QTDIR/include/QtGui"
          fi
        fi
      fi
    fi
    
		# Find Qt in default location.
	if test x"${ac_cv_path_qt4_incl}" = x; then
		if test -d /usr/include/qt4; then
			if test -d /usr/include/qt4/QtCore; then
			qt_base='/usr/include/qt4'
			# Try to find the latest version.
			ac_cv_path_qt4_incl="-I${qt_base} -I${qt_base}/QtCore -I${qt_base}/QtGui"
			fi
		fi
	fi
  
  if test x"${ac_cv_path_qt4_incl}" = x; then
		if test -d /usr/share/qt4; then
			if test -d /usr/share/qt4/QtCore; then
			qt_base = /usr/share/qt4
		  # Try to find the latest version.
		  ac_cv_path_qt4_incl="-I${qt_base}/include -I${qt_base}/include/QtCore -I${qt_base}/include/QtGui"
		  fi
		fi
	fi

     AC_MSG_CHECKING([for QT 4.x headers])
    if test x$cross_compiling = xno; then
      if test x"${PKG_CONFIG}" != x -a x"${ac_cv_path_qt4_incl}" = x; then
        ${PKG_CONFIG} --exists  QtCore QtGui && ac_cv_path_qt4_incl="`$PKG_CONFIG --cflags-only-I QtCore QtGui`"
	qt4_topdir=`echo "${ac_cv_path_qt4_incl}" | sed -e 's/-I//g' | cut -d ' ' -f 1`
      fi
    fi

    dnl QT 4.x stores it's headers in ../lib/qt4/include for example, so
    dnl use libslist instead of incllist
    if test x"${ac_cv_path_qt4_incl}" = x; then
      for i in ${incllist} ${libslist}; do
        dnl Some distributions put the QT headers directly in the directory
        if test -f $i/Qt/qobject.h; then
          qt4_topdir="$i"
	  if test x"$i" != x"/usr/include"; then
            ac_cv_path_qt4_incl="-I$i"
	  fi
          break
        fi
        
        
	dnl Some distributions put the QT headers in a sub directory
        if test -f $i/qt4/Qt/qobject.h; then
          qt4_topdir="$i/qt4"
          ac_cv_path_qt4_incl="-I$i/qt4"
          break
        fi
      done
    fi

    dnl this is a list of al the libraries supported by QT 4.x, but we don't need all of
    dnl then, but we might as well get all the paths, as header files ofteninclude other
    dnl header files.
    all_qt4_libs="QtCore QtGui QtOpenGL QtXml QtDBus QtNetwork QtScript QtSql QtTest QtSvg QtWebKit"
    for i in ${all_qt4_libs}; do
      dnl Darwin is easy, everything is in the same location on all machines.
      if test x"${darwin}" = xyes; then
        if test -d /Library/Frameworks; then
	  ac_cv_path_qt4_incl="${ac_cv_path_qt4_incl} /Library/Frameworks/$i.framework/Headers"
	fi
      else
        if test -d ${qt4_topdir}/$i; then
	  ac_cv_path_qt4_incl="${ac_cv_path_qt4_incl} -I${qt4_topdir}/$i"
	fi
      fi
    done
    
    if test x"${ac_cv_path_qt4_incl}" = x; then
      QT4_CFLAGS=""
      AC_MSG_RESULT(no)
    else
      QT4_CFLAGS="${ac_cv_path_qt4_incl}"
      AC_MSG_RESULT(${ac_cv_path_qt4_incl})
    fi
    
    

  dnl Look for the libraries
    AC_ARG_WITH(qt4_lib, AC_HELP_STRING([--with-qt4-lib=DIR], [directory where QT 4.x libraries are]), with_qt4_lib=${withval})
    AC_CACHE_VAL(ac_cv_path_qt4_lib, [
      if test x"${with_qt4_lib}" != x ; then
        if test `ls -C1 ${with_qt4_lib}/libQtCore.${shlibext}* | wc -l` -gt 0 ; then
         ac_cv_path_qt4_lib="-L`(cd ${with_qt4_lib}; pwd)` ${qt4support} -lQtCore -lQtGui"
        else
          AC_MSG_WARN([${with_qt4_lib} directory doesn't contain QT 4.x libraries.])
        fi
      fi
    ])

    if test x$cross_compiling = xno; then
      if test x"$PKG_CONFIG" != x -a x"${ac_cv_path_qt4_lib}" = x; then
        $PKG_CONFIG --exists QtCore QtGui && ac_cv_path_qt4_lib="`$PKG_CONFIG --libs QtCore QtGui`"
      fi
    fi
    
    if test x"${ac_cv_path_qt4_lib}" = x; then
      dnl if QTDIR is set in the users environment, use that, as that's what
      dnl most QT programmers do, as it's required by the QT build system.
      if test x$QTDIR != x; then
        if test -f $QTDIR/lib/libQtCore.so; then
          qt_base=`basename $QTDIR`
          dnl Only support version 4 in this file.
          if test x"${qt_base}" != x"qt4"; then
            AC_MSG_WARN([QT 4.x is required])
          else
          	ac_cv_path_qt4_lib="-L$QTDIR/lib -lQtCore -lQtGui"
          fi
        fi
      fi
    fi
    
    # Find Qt in default location.
	if test x"${ac_cv_path_qt4_lib}" = x; then
		if test -d /usr/lib/qt4; then
			if test -f /usr/lib/libQtCore.so; then
				ac_cv_path_qt4_lib="-L/usr/lib -lQtCore -lQtGui"
			fi
		fi
	fi

    AC_MSG_CHECKING([for QT 4.x libraries])
    if test x"${ac_cv_path_qt4_lib}" = x; then
      for i in $libslist; do
        if test -f $i/libQtCore.${shlibext}; then
	  qt4_topdir=$i
	  if test x"$i" != x"/usr/lib"; then
	    ac_cv_path_qt4_lib="-L$i"
	  fi
	  break
	fi
      done
      dnl redefine the list of libraries, as we don't need all of them, so why bother.
      all_qt4_libs="QtCore QtGui"
      for i in ${all_qt4_libs}; do
        if test -f ${qt4_topdir}/lib$i.${shlibext}; then
	    ac_cv_path_qt4_lib="${ac_cv_path_qt4_lib} -l$i"
	fi
      done
    fi

    if test x"${ac_cv_path_qt4_lib}" != x -a x"${ac_cv_path_qt4_incl}" != x; then
	    AC_MSG_RESULT(${ac_cv_path_qt4_lib})
	    QT4_LIBS="${ac_cv_path_qt4_lib}"
	    AC_DEFINE(HAVE_QT4, 1, [Have QT 4.x installed])
	    has_qt4="yes"
	  else
	    AC_MSG_RESULT(no)
	    QT4_CFLAGS=""
	    QT4_LIBS=""
	    has_qt4="no"
    fi
    
    dnl Look for the header
  AC_ARG_WITH(qt4_moc, AC_HELP_STRING([--with-qt4-moc=DIR],
             [directory where QT 4.x MOC file is located]),
	     with_qt4_moc=${withval})
	     
	     dnl Look for the header
dnl  AC_ARG_WITH(qt4_incl, AC_HELP_STRING([--with-qt4-uic=DIR],
dnl            [directory where QT 4.x headers are]),
dnl	     with_qt4_uic=${withval})

    AC_PATH_PROGS(MOC4, [moc-qt4 moc], ,[${with_qt4_mocl} ${QTDIR}/bin /usr/lib/qt4/bin /usr/share/qt4/bin/ /opt/local/bin])
dnl    AC_PATH_PROGS(UIC4, [uic-qt4 uic], ,[${with_qt4_uic} ${QTDIR}/bin /usr/lib/qt4/bin /usr/share/qt4/bin/])


    AC_SUBST([QT4_CFLAGS])
    AC_SUBST([QT4_LIBS])
    AC_SUBST([MOC4])
dnl   AC_SUBST([UIC4])
                          
])				
