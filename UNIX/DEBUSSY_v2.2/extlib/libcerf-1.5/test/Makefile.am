# Library libcerf:
#   Compute complex error functions, based on a new implementation of
#   Faddeeva's w_of_z. Also provide Dawson and Voigt functions.
# 
# File test/Makefile.am:
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

AM_CFLAGS += -I$(srcdir)/../lib
AM_LDFLAGS = -lm -L../lib -lcerf

noinst_PROGRAMS = runvoigt run_w_of_z
TESTS = test_libcerf_1
check_PROGRAMS = $(TESTS)

test_libcerf_1_SOURCES = test1.c
runvoigt_SOURCES = runvoigt.c
run_w_of_z_SOURCES = run_w_of_z.c
