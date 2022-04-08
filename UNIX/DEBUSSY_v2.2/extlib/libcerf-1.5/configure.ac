# Library libcerf:
#   Compute complex error functions, based on a new implementation of
#   Faddeeva's w_of_z. Also provide Dawson and Voigt functions.
# 
# File configure.ac:
#   Input for the build process using GNU autotools.
# 
# Copyright:
#   (C) 2012 Massachusetts Institute of Technology
#   (C) 2013 Forschungszentrum Jülich GmbH
# 
# Licence:
#   MIT Licence. See ../COPYING.
# 
# Authors:
#   Steven G. Johnson, Massachusetts Institute of Technology, 2012, core author
#   Joachim Wuttke, Forschungszentrum Jülich, 2013, package maintainer
# 
# Website:
#   http://apps.jcns.fz-juelich.de/libcerf

AC_INIT([libcerf],[1:5],[http://apps.jcns.fz-juelich.de/libcerf])
  # version is current:revision:age; for simplicity, we always put age=0
  # <- http://www.sourceware.org/autobook/autobook/autobook_91.html

# standard configuration for shared libraries
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([foreign]) # don't insert GNU standard text files
LT_INIT([disable-static])

AC_PROG_CC_C99
AC_SUBST(AM_CFLAGS,"-pedantic -Wall")
AC_CONFIG_HEADERS([config.h]) # to avoid endless -D options
AC_HEADER_STDC

# consistency check: is source code present?
AC_CONFIG_SRCDIR([lib/w_of_z.c]) # assure we have the right src dir

# dependency checks
AC_CHECK_LIB([m], [sin], , [AC_MSG_ERROR(libm not found)])
AC_CHECK_LIB([m], [erfc], , [AC_MSG_ERROR(libm outdated: missing erfc)])

# make these Makefiles
AC_CONFIG_FILES([Makefile lib/Makefile man/Makefile test/Makefile libcerf.pc])
AC_OUTPUT
