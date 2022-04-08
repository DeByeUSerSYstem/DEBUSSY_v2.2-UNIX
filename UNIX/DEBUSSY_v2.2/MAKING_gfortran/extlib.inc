LIBDIREXT1 = /Users/federica/DEBUSSY/DEBUSSY_v2.2/extlib/libcerf-1.5/lib/.libs
LIBDIREXT2 = /Users/federica/DEBUSSY/DEBUSSY_v2.2/extlib/nlopt-2.4.2/.libs
LIBDIREXT3 = /Users/federica/DEBUSSY/DEBUSSY_v2.2/extlib/LAPACK95-lite
INCLDIREXT1 = /Users/federica/DEBUSSY/DEBUSSY_v2.2/extlib/LAPACK95-lite/lapack95_modules
####
LIB1 = -L$(LIBDIREXT1) -lcerf
LIB2 = -L$(LIBDIREXT2) -lm -lnlopt
LIB3 = -L$(LIBDIREXT3) -llapack95 -llapack -lblas
