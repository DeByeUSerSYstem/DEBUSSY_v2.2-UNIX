### program path
debpath="$SRC_DIR/UNIX/DEBUSSY_v2.2/"
pgmpath="$SRC_DIR/UNIX/DEBUSSY_v2.2/"
###
### paths
binpath=$pgmpath'bin'
libpath=$pgmpath'lib'
modpath=$pgmpath'incl'
#################
if [ -d lib ]; then
    echo '* lib folder exists'
else
    mkdir lib
fi
if [ -d incl ]; then
    echo '* incl folder exists'
else
    mkdir incl
fi
if [ -d bin ]; then
    echo '* bin folder exists'
else
    mkdir bin
fi
#
##__extpath.sh
a=$debpath'ext_database/EPDL97/'
echo 'character(256),save :: path_EPDL97= &' > $debpath\src/Debussy-src/EPDL97.inc
echo '"'$a'"' >> $debpath\src/Debussy-src/EPDL97.inc
#
a=$debpath'ext_database/SpaceGroups/'
echo 'character(256),save :: path_SpaceGroups= &' >> $debpath\src/Debussy-src/EPDL97.inc
echo '"'$a'"' >> $debpath\src/Debussy-src/EPDL97.inc
#
cp $debpath\src/Debussy-src/EPDL97.inc $debpath/MAKING_gfortran/.
##__end
#
##__movesrch_D.txt
srcdir=$debpath'src/Debussy-src/'
cp $srcdir'local_system.inc' .
N=0
while read LINE ; do
    N=$((N+1))
    echo "Line $N = $LINE : copy here"
    cp $srcdir$LINE'.f90' .
done < listsrc_D.txt
##__end
echo '__Sources for library are here:'
ls -rt1 *90 *.inc
echo '__Removing existing modules:'
rm -vf $modpath/*.mod
echo '__MAKING Debussy ...'
sh MakeDebussy.sh
cp err.log err1.log
##__clearsrc_D.txt
N=0
while read LINE ; do
    N=$((N+1))
    echo "Line $N = $LINE"
    rm $LINE'.f90'
done < listsrc_D.txt
rm local_system.inc
##__end
echo '** Sources cleared'
# ls -rt1
mv Debussy $binpath/
##__movesrch_C.txt
srcdir=$debpath'src/Claude-src/'
N=0
while read LINE ; do
    N=$((N+1))
    echo "Line $N = $LINE : copy here"
    cp $srcdir$LINE'.f90' .
done < listsrc_C.txt
##__end
sh MakeClaude.sh
cp err.log err2.log
##__clearsrc_C.txt
N=0
while read LINE ; do
    N=$((N+1))
    echo "Line $N = $LINE"
    rm $LINE'.f90'
done < listsrc_C.txt
##__end
fl=$(cat listsrc_C.txt)
for f in $fl; do
    mv $f $binpath/
done
# mv ./lib/* $libpath/
# mv ./incl/* $modpath/
echo '-----------------------------------------------------'
head -n 99999 err?.log

echo 'DEBUSSY build info' > $debpath/build.txt
echo ''
date >> $debpath/build.txt

echo 'done...'
