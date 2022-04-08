#!/bin/bash

 olddir='/Volumes/TOSHIBA_TOSCA/DEBUSSY/DEBUSSY_v2.2_2019/src_2/Claude-src'
 newdir='/Volumes/TOSHIBA_TOSCA/DEBUSSY/PackD'
 diffdir='/Volumes/TOSHIBA_TOSCA/DEBUSSY/DEBUSSY_v2.2_2019/src_2/DIFFs'
# olddir='/Users/federica/DEBUSSY/DEBUSSY_v2.1beta_Apr2017/src/Claude-src'
# newdir='/Users/federica/DEBUSSY/DEBUSSY_v2.1beta_Apr2017/AC_04052017/src/Claude-src'
# diffdir='/Users/federica/DEBUSSY/DEBUSSY_v2.1beta_Apr2017/DIFF05042017'
N=0
while read LINE ; do
	N=$((N+1))
#	echo "Line $N = $LINE"
#	echo $olddir'/'$LINE'.f90' $newdir'/'$LINE'.f90' $diffdir'/'$LINE'.diff'
	diff -b $olddir'/'$LINE'.f90' $newdir'/'$LINE'.f90' > $diffdir'/'$LINE'.diff'
	FILENAME=$LINE'.diff'
    eval $(stat -s $diffdir'/'$FILENAME)
    echo "Size of $FILENAME = $st_size bytes."
    if [ $st_size -eq 0 ]
    then
      rm $diffdir'/'$FILENAME
    fi
done < listsrc_C.txt
 olddir='/Volumes/TOSHIBA_TOSCA/DEBUSSY/DEBUSSY_v2.2_2019/src_2/Debussy-src'
 newdir='/Volumes/TOSHIBA_TOSCA/DEBUSSY/PackD'
 diffdir='/Volumes/TOSHIBA_TOSCA/DEBUSSY/DEBUSSY_v2.2_2019/src_2/DIFFs'
# olddir='/Users/federica/DEBUSSY/DEBUSSY_v2.1beta_Apr2017/src/Debussy-src'
# newdir='/Users/federica/DEBUSSY/DEBUSSY_v2.1beta_Apr2017/AC_04052017/src/Debussy-src'
# diffdir='/Users/federica/DEBUSSY/DEBUSSY_v2.1beta_Apr2017/DIFF05042017'
N=0
while read LINE ; do
	N=$((N+1))
#	echo "Line $N = $LINE"
#	echo $olddir'/'$LINE'.f90' $newdir'/'$LINE'.f90' $diffdir'/'$LINE'.diff'
	diff -b $olddir'/'$LINE'.f90' $newdir'/'$LINE'.f90' > $diffdir'/'$LINE'.diff'
	FILENAME=$LINE'.diff'
    eval $(stat -s $diffdir'/'$FILENAME)
    echo "Size of $FILENAME = $st_size bytes."
    if [ $st_size -eq 0 ]
    then
      rm $diffdir'/'$FILENAME
    fi
done < listsrc_D.txt
