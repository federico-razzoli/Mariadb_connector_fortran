Fortran wrapper library for MariaDB Connector/C

It can use to connect to MariaDB, MySQL, Oracle.

Download newest MariaDB client connector/C (dll file) here for Windows OS:

https://mariadb.com/kb/en/mariadb-connector-c/

Compile test file with gfortran in MSYS2 for windows os (may be needed some package for MariaDB Connector/C header).

gfortran -Wall -static-libgfortran -static-libgcc -static-libstdc++ -O3 mariadb_test.f90  libmariadb_fortran.f90 c_util.f90 -lWs2_32 -lmariadb -o mariadb_test
