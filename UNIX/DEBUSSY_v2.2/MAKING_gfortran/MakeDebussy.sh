make -f Make_DEBUSSY.txt clean
make -f Make_DEBUSSY.txt libclean
make -f Make_DEBUSSY.txt modclean
make -f Make_DEBUSSY.txt libDebussy.a
make -f Make_DEBUSSY.txt Debussy
make -f Make_DEBUSSY.txt install
rm *.o
