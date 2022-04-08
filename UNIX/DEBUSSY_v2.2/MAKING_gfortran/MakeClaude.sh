make -f Make_CLAUDE.txt clean
#
N=0
while read LINE ; do
	N=$((N+1))
	echo "Line $N = $LINE : making..."
	make -f Make_CLAUDE.txt $LINE
done < listsrc_C.txt
cp err.log err2.log
rm *.o *.mod err.log 
