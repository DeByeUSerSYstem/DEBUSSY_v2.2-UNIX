#!/bin/bash
cd ..
#
a=$PWD'/ext_database/EPDL97/'
echo 'character(256),save :: path_EPDL97= &' > ./src/Debussy-src/EPDL97.inc
echo '"'$a'"' >> ./src/Debussy-src/EPDL97.inc
#
a=$PWD'/ext_database/SpaceGroups/'
echo 'character(256),save :: path_SpaceGroups= &' >> ./src/Debussy-src/EPDL97.inc
echo '"'$a'"' >> ./src/Debussy-src/EPDL97.inc
#
cp src/Debussy-src/EPDL97.inc ./MAKING_gfortran
