Fortran wrapper library for Mariadb-connector/C 

Download dll file here for windows OS:

https://mariadb.com/kb/en/mariadb-connector-c/

Compile with gfortran in MSYS2 for windows os (may be need some package for Mariadb-connector/C header)
gfortran -Wall -static-libgfortran -static-libgcc -static-libstdc++ -O3 mariadb_test.f90  libmariadb_fortran.f90 c_util.f90 -lWs2_32 -lmariadb -o mariadb_test
