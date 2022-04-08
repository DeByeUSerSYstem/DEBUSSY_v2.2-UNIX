# Library libcerf:
#   Compute complex error functions, based on a new implementation of
#   Faddeeva's w_of_z. Also provide Dawson and Voigt functions.
# 
# File man/Makefile.am:
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

man_MANS = w_of_z.3 im_w_of_x.3 dawson.3 cdawson.3 cerf.3 cerfc.3 erfcx.3 cerfcx.3 erfi.3 cerfi.3 voigt.3 
html_DATA = w_of_z.html voigt.html dawson.html cerf.html erfcx.html erfi.html

w_of_z.3 : w_of_z.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

im_w_of_x.3 : w_of_z.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

dawson.3 : dawson.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

cdawson.3 : dawson.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

cerf.3 : cerf.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

cerfc.3 : cerf.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

erfcx.3 : erfcx.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

cerfcx.3 : erfcx.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

erfi.3 : erfi.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

cerfi.3 : erfi.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

voigt.3 : voigt.pod
	pod2man -s 3 -c "libcerf manual" $< > $@

%.html : %.pod
	pod2html --title="libcerf - Complex error, Faddeeva, Dawson, and Voigt functions" --noindex $< > $@
