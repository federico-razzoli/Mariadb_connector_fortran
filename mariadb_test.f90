! mariadb_test.f90
! 1-1-2022
! Author:  Vu Hoang Giang
! GitHub:  https://github.com/v-h-giang/Mariadb_connector_fortran
! Licence: ISC
program mariadb_test
  use :: mariadb_fortran
  implicit none

  CHARACTER(len = :),ALLOCATABLE                    :: comm
  TYPE(DATABASE_CONNECT_INFO)                       :: mydatabase

  mydatabase = DATABASE("127.0.0.1","giang","",3306)
  ! call EXECUTE_COMMAND_LINE("CHCP 65001")
  comm = "SHOW DATABASES" 
  call mydatabase%RUN(comm)
  print *, "enter to go next"
  read(*,*) 

  comm = "USE test_group" 
  call mydatabase%RUN(comm)
  print *, "enter to go next"
  read(*,*)

  comm = "SELECT * FROM test_group.employees" 
  call mydatabase%RUN(comm)
  print *, "enter to go next"
  read(*,*)
end program mariadb_test