! mariadb_fortran.f90
!
! Fortran wrapper library for Mariadb-connector/C 
! Download dll file here for windows OS:
! https://mariadb.com/kb/en/mariadb-connector-c/
! 1-1-2022
! Author:  Vu Hoang Giang
! GitHub:  https://github.com/v-h-giang/Mariadb_connector_fortran
! Licence: MIT

module mariadb_fortran
      use,intrinsic :: iso_c_binding
      use :: c_util ,only : c_uint64_t,c_unsigned_char,c_unsigned_long
      implicit none

      INTEGER, PARAMETER                                     :: UINT = C_INT, DWORD = C_LONG
      INTEGER, PARAMETER                                     :: MYSQL_ERRMSG_SIZE = 512
      INTEGER, PARAMETER                                     :: SQLSTATE_LENGTH = 5
      CHARACTER(kind=c_char, len=*), PARAMETER               :: LOCAL_HOST = "localhost"

      INTEGER, PARAMETER                                     :: MYSQL_OPT_NONBLOCK = 6000

      INTEGER, PARAMETER                                     :: MYSQL_WAIT_READ    = 1
      INTEGER, PARAMETER                                     :: MYSQL_WAIT_WRITE   = 2
      INTEGER, PARAMETER                                     :: MYSQL_WAIT_EXCEPT  = 4
      INTEGER, PARAMETER                                     :: MYSQL_WAIT_TIMEOUT = 8


      INTEGER, PARAMETER                                     :: POLLRDNORM = 256
      INTEGER, PARAMETER                                     :: POLLRDBAND = 512
      INTEGER, PARAMETER                                     :: POLLIN     = IOR(POLLRDNORM,POLLRDBAND)
      INTEGER, PARAMETER                                     :: POLLPRI    = 1024

      INTEGER, PARAMETER                                     :: POLLWRNORM = 16
      INTEGER, PARAMETER                                     :: POLLOUT    = POLLWRNORM
      INTEGER, PARAMETER                                     :: POLLWRBAND = 32

      INTEGER, PARAMETER                                     :: POLLERR    = 1
      INTEGER, PARAMETER                                     :: POLLHUP    = 2
      INTEGER, PARAMETER                                     :: POLLNVAL   = 4

      TYPE ::  db_field_in_row
            CHARACTER(len = : ), ALLOCATABLE                :: text
      END TYPE db_field_in_row

      TYPE ::  DATABASE_CONNECT_INFO
            CHARACTER(len = : ), ALLOCATABLE                :: IP_ADDRESS
            CHARACTER(len = : ), ALLOCATABLE                :: USERNAME
            CHARACTER(len = : ), ALLOCATABLE                :: PASSWORD 
            INTEGER                                         :: PORT_ID = 3306
            TYPE(db_field_in_row),DIMENSION(:,:),ALLOCATABLE:: DATA 
            CONTAINS 
            PROCEDURE :: RUN 
      END TYPE DATABASE_CONNECT_INFO

      TYPE, bind(c) ::  MA_MEM_ROOT
            TYPE(c_ptr)                                      ::   free ! MA_USED_MEM *free
            TYPE(c_ptr)                                      ::  used ! MA_USED_MEM *used
            TYPE(c_ptr)                                      ::  pre_alloc ! MA_USED_MEM *pre_alloc
            INTEGER(c_uint64_t)                              ::  min_malloc  ! size_t min_malloc
            INTEGER(c_uint64_t)                              ::  block_size  ! size_t block_size
            INTEGER(DWORD)                                   ::  block_num  ! unsigned int block_num
            INTEGER(DWORD)                                   ::  first_block_usage  ! unsigned int first_block_usage
            type(c_funptr)                                   ::  error_handler
           ! void (*error_handler)(void)
      END TYPE MA_MEM_ROOT

      TYPE,bind(c) :: NET
            TYPE(c_ptr)                                      ::   pvio    !  MARIADB_PVIO *pvio
            INTEGER(c_unsigned_char)                         ::   buff    !  unsigned char *buff
            INTEGER(c_unsigned_char)                         ::   buff_end ,write_pos, read_pos  ! unsigned char *buff_end,*write_pos,*read_pos
            INTEGER(c_uint64_t)                              ::   fd       !  my_socket fd					/* For Perl DBI/dbd */
            INTEGER(c_unsigned_long)                         ::   remain_in_buf,length         !  unsigned long remain_in_buf,length
            INTEGER(c_unsigned_long)                         ::   buf_length, where_b         !  unsigned long buf_length, where_b
            INTEGER(c_unsigned_long)                         ::   max_packet, max_packet_size         !  unsigned long max_packet, max_packet_size
            INTEGER(DWORD)                                   ::   pkt_nr, compress_pkt_nr   !  unsigned int pkt_nr, compress_pkt_nr
            INTEGER(DWORD)                                   ::   write_timeout , read_timeout, retry_count 
                                                                   !  unsigned int write_timeout, read_timeout, retry_count
            INTEGER(C_INT)                                   ::   fcntl     !  int fcntl
            TYPE(c_ptr)                                      ::   return_status   !  unsigned int *return_status
            INTEGER(c_unsigned_char)                         ::   reading_or_writing        !  unsigned char reading_or_writing
            CHARACTER(c_char)                                ::   save_char    !  char save_char
            CHARACTER(c_char)                                ::   unused_1    !  char unused_1
                                                                              !typedef char my_bool
            CHARACTER(c_char)                                ::   unused_2   !  my_bool unused_2
            CHARACTER(c_char)                                ::   compress   !  my_bool compress
            CHARACTER(c_char)                                ::   unused_3   !  my_bool unused_3
            TYPE(c_ptr)                                      ::   unused_4     !  void *unused_4
            INTEGER(DWORD)                                   ::   last_errno   !  unsigned int last_errno
            INTEGER(c_unsigned_char)                         ::   error        !  unsigned char error
            CHARACTER(c_char)                                ::   unused_5   !  my_bool unused_5
            CHARACTER(c_char)                                ::   unused_6   !  my_bool unused_6
            CHARACTER(c_char),DIMENSION(MYSQL_ERRMSG_SIZE)   ::   last_error    !  char last_error[MYSQL_ERRMSG_SIZE]
            CHARACTER(c_char),DIMENSION(SQLSTATE_LENGTH+1)   ::   sqlstate    !  char sqlstate[SQLSTATE_LENGTH+1]
            TYPE(c_ptr)                                      ::   extension    !  struct st_mariadb_net_extension *extension
      END TYPE NET

      TYPE,BIND(C) :: LIST
            type(c_ptr)   :: prev, next
            type(c_ptr)   :: data
      END TYPE LIST
      TYPE,BIND(C) :: st_mysql_options
            INTEGER(UINT)                                          :: connect_timeout, read_timeout, write_timeout
            INTEGER(UINT)                                          :: port, protocol
            INTEGER(c_unsigned_long)                               :: client_flag
            type(c_ptr)                                            :: host,user,password,unix_socket,db
            type(c_ptr)                                            :: init_command
            type(c_ptr)                                            :: my_cnf_file,my_cnf_group, charset_dir, charset_name
            type(c_ptr)                                            :: ssl_key              !PEM key file 
            type(c_ptr)                                            :: ssl_cert               ! PEM cert file 
            type(c_ptr)                                            :: ssl_ca               ! PEM CA file 
            type(c_ptr)                                            :: ssl_capath               ! PEM directory of CA-s? 
            type(c_ptr)                                            :: ssl_cipher
            type(c_ptr)                                            :: shared_memory_base_name
            INTEGER(c_unsigned_long)                               :: max_allowed_packet
            CHARACTER(c_char)                                      :: use_ssl
            CHARACTER(c_char)                                      :: compress,named_pipe
            CHARACTER(c_char)                                      :: reconnect, unused_1, unused_2, unused_3
            INTEGER(c_int)                                         :: methods_to_use
            type(c_ptr)                                            :: bind_address
            CHARACTER(c_char)                                      :: secure_auth
            CHARACTER(c_char)                                      :: report_data_truncation

            ! /* function pointers for local infile support */
            ! int (*local_infile_init)(void **, const char *, void *)
            ! int (*local_infile_read)(void *, char *, unsigned int)
            ! void (*local_infile_end)(void *)
            ! int (*local_infile_error)(void *, char *, unsigned int)
            ! void *local_infile_userdata
            ! struct st_mysql_options_extension *extension
      END TYPE st_mysql_options


      TYPE,bind(c) :: MYSQL
            TYPE(NET)                                 :: net 
            type(c_ptr)                               :: unused_0
            type(c_ptr)                               :: host,user,passwd,unix_socket,server_version,host_info 
            type(c_ptr)                               :: info,db  ! char *info,*db
            type(c_ptr)                               :: charset  ! const struct ma_charset_info_st *charset   ! character set 
            type(c_ptr)                               :: fields                    ! MYSQL_FIELD *fields
            type(MA_MEM_ROOT)                         :: field_alloc
            INTEGER(c_uint64_t)                       :: affected_rows   ! unsigned long_ long  affected_rows
            INTEGER(c_uint64_t)                       :: insert_id   ! unsigned long_ long insert_id	
            INTEGER(c_uint64_t)                       :: extra_info   ! unsigned long_ long extra_info
            INTEGER(DWORD)                            :: thread_id
            INTEGER(DWORD)                            :: packet_length
            INTEGER(UINT)                             :: port  ! unsigned int port
            INTEGER(DWORD)                            :: client_flag
            INTEGER(DWORD)                            :: server_capabilities
            INTEGER(DWORD)                            :: protocol_version ! unsigned int protocol_version
            INTEGER(DWORD)                            :: field_count     ! unsigned int field_count
            INTEGER(DWORD)                            :: server_status     ! unsigned int server_status
            INTEGER(DWORD)                            :: server_language     ! unsigned int server_language
            INTEGER(DWORD)                            :: warning_count     ! unsigned int warning_count    
            type(st_mysql_options)                    :: options ! struct st_mysql_options options
            INTEGER(c_int)                            :: status  ! enum mysql_status status
            CHARACTER(c_char)                         :: free_me       ! my_bool	free_me	
            CHARACTER(c_char)                         :: unused_1       ! my_bool	unused_1
            CHARACTER(C_char)                         :: scramble_buff(21)               ! char  scramble_buff[20+ 1]
             ! madded after 3.23.58  !
            CHARACTER(c_char)                         :: unused_2        ! my_bool unused_2
            type(c_ptr)                               :: unused_3,unused_4,unused_5,unused_6   
            TYPE(LIST)                                :: stmts          ! LIST          *stmts
            type(c_ptr)                               :: methods  ! const struct  st_mariadb_methods *methods
            type(c_ptr)                               :: thd   ! void          *thd
            type(c_ptr)                               :: unbuffered_fetch_owner       ! my_bool  *unbuffered_fetch_owner
            type(c_ptr)                               :: info_buffer        ! char          *info_buffer
            type(c_ptr)                               :: extension  ! struct st_mariadb_extension *extension
      END TYPE MYSQL

      TYPE, bind(c) ::  MYSQL_RES
            INTEGER(c_uint64_t)                       :: row_count
            INTEGER(DWORD)                            :: field_count, current_field
            type(c_ptr)                               :: fields  ! MYSQL_FIELD *fields
            type(c_ptr)                               :: data !MYSQL_DATA *data
            type(c_ptr)                               :: data_cursor  !MYSQL_ROWS *data_cursor
            type(MA_MEM_ROOT)                         :: field_alloc !MA_MEM_ROOT field_alloc
            type(c_ptr)                               :: row  
            type(c_ptr)                               :: current_row
            type(c_ptr)                               :: lengths !unsigned long *lengths ! column lengths of current row 
            type(c_ptr)                               :: handle  !MYSQL *handle ! for unbuffered reads 
            CHARACTER(c_char)                         :: eof !my_bool eof ! Used my mysql_fetch_row 
            CHARACTER(c_char)                         :: is_ps !my_bool is_ps
      END TYPE MYSQL_RES

      TYPE,BIND(C) :: MYSQL_ROWS
            type(c_ptr)                               :: next !struct st_mysql_rows *next !list of rows 
            type(c_ptr)                               :: data ! typedef char **MYSQL_ROW 
            INTEGER(c_unsigned_long)                  :: length
      END TYPE MYSQL_ROWS  

      TYPE,BIND(C) :: pollfd 
            INTEGER(c_uint64_t)                       :: fd
            INTEGER(c_short)                          :: events
            INTEGER(c_short)                          :: revents
      END TYPE pollfd

      INTERFACE

      ! int STDCALL mysql_server_init(int argc, char **argv, char **groups)
      FUNCTION mysql_library_init(argc, argv, groups) bind(c,name = "mysql_server_init")
            import :: c_ptr , MYSQL,c_int
            implicit none
            INTEGER(c_int),VALUE                :: argc
            type(c_ptr)                         :: argv 
            type(c_ptr)                         :: groups
            
            !GCC$ ATTRIBUTES STDCALL            :: mysql_library_init   
            INTEGER(c_int)                      :: mysql_library_init
      END FUNCTION mysql_library_init


       ! MYSQL * STDCALL mysql_init(MYSQL *mysql)
      FUNCTION mysql_init(mysql_) bind(c,name = "mysql_init")
            import :: c_ptr , MYSQL
            implicit none
            type(MYSQL),OPTIONAL                :: mysql_
            !GCC$ ATTRIBUTES STDCALL            :: mysql_init   
            type(c_ptr)                         :: mysql_init
      END FUNCTION mysql_init
      
      ! void STDCALL mysql_close(MYSQL *sock)
      SUBROUTINE mysql_close(sock_sql) bind(c,name = "mysql_close")
            import :: c_ptr , MYSQL
            implicit none
            type(MYSQL)                         :: sock_sql
            !GCC$ ATTRIBUTES STDCALL            :: mysql_close   
      END SUBROUTINE mysql_close

       ! void (*error_handler)(void)
      FUNCTION  error_handler() bind(c,name = "error_handler")
            import :: c_ptr
            implicit none
             !GCC$ ATTRIBUTES STDCALL            :: error_handler   
            type(c_ptr)                         :: error_handler
      END FUNCTION

      ! MYSQL *		STDCALL mysql_real_connect(MYSQL *mysql, const char *host,
	! 				   const char *user,
	! 				   const char *passwd,
	! 				   const char *db,
	! 				   unsigned int port,
	! 				   const char *unix_socket,
	! 				   unsigned long clientflag)
      FUNCTION mysql_real_connect(mysql_, host, user, password,&
             db, port, unix_socket,clientflag) bind(c,name = "mysql_real_connect")
            import :: c_ptr , MYSQL,c_char,c_int, c_unsigned_long
            implicit none
            type(MYSQL)                         :: mysql_
            CHARACTER(C_CHAR),DIMENSION(*)      :: host
            CHARACTER(C_CHAR),DIMENSION(*)      :: user
            CHARACTER(C_CHAR),DIMENSION(*)      :: password
            CHARACTER(C_CHAR),DIMENSION(*)      :: db
            INTEGER(c_int),VALUE                :: port
            CHARACTER(C_CHAR),DIMENSION(*)      :: unix_socket
            INTEGER(c_unsigned_long),VALUE      :: clientflag

            !GCC$ ATTRIBUTES STDCALL            :: mysql_real_connect   
            type(c_ptr)                         :: mysql_real_connect
      END FUNCTION mysql_real_connect

      ! int         STDCALL mysql_real_connect_start(MYSQL **ret, MYSQL *mysql,
      !                                            const char *host,
      !                                            const char *user,
      !                                            const char *passwd,
      !                                            const char *db,
      !                                            unsigned int port,
      !                                            const char *unix_socket,
      !                                            unsigned long clientflag)

      FUNCTION mysql_real_connect_start(ret,mysql_, host, user, password,&
            db, port, unix_socket,clientflag) bind(c,name = "mysql_real_connect_start")
           import :: c_ptr , MYSQL,c_char,c_int, c_unsigned_long
           implicit none
           type(c_ptr)                                      :: ret  
           type(MYSQL)                                      :: mysql_
           CHARACTER(C_CHAR),DIMENSION(*)                   :: host
           CHARACTER(C_CHAR),DIMENSION(*)                   :: user
           CHARACTER(C_CHAR),DIMENSION(*),OPTIONAL          :: password
           CHARACTER(C_CHAR),DIMENSION(*),OPTIONAL          :: db
           INTEGER(c_int),VALUE                             :: port
           CHARACTER(C_CHAR),DIMENSION(*),OPTIONAL          :: unix_socket
           INTEGER(c_unsigned_long),VALUE                   :: clientflag

           !GCC$ ATTRIBUTES STDCALL                         :: mysql_real_connect_start   
           INTEGER(c_int)                                   :: mysql_real_connect_start
      END FUNCTION mysql_real_connect_start
      
      ! int	  mysql_options(MYSQL *mysql,enum mysql_option option, const void *arg)
      FUNCTION mysql_options(mysql_, option, arg) bind(c,name = "mysql_options")
            import :: c_ptr , MYSQL,c_int
            implicit none
            type(MYSQL)                         :: mysql_
            INTEGER(c_int),VALUE                :: option
            type(c_ptr),VALUE                   :: arg
            
            !GCC$ ATTRIBUTES STDCALL            :: mysql_options   
            INTEGER(c_int)                      :: mysql_options
      END FUNCTION mysql_options

      ! int   STDCALL mysql_real_connect_cont(MYSQL **ret, MYSQL *mysql,int status)
      FUNCTION mysql_real_connect_cont(ret,mysql_, status) bind(c,name = "mysql_real_connect_cont")
            import :: c_ptr , MYSQL,c_int
            implicit none
            type(c_ptr)                         :: ret  
            type(MYSQL)                         :: mysql_
            INTEGER(c_int),VALUE                :: status
            
            !GCC$ ATTRIBUTES STDCALL            :: mysql_real_connect_cont   
            INTEGER(c_int)                      :: mysql_real_connect_cont
      END FUNCTION mysql_real_connect_cont
      
      ! my_socket STDCALL mysql_get_socket(MYSQL *mysql)
      FUNCTION mysql_get_socket(mysql_) bind(c,name = "mysql_get_socket")
            import :: c_uint64_t , MYSQL
            implicit none
            type(MYSQL)                         :: mysql_
            !GCC$ ATTRIBUTES STDCALL            :: mysql_get_socket   
            INTEGER(c_uint64_t)                 :: mysql_get_socket ! unsigned long long
      END FUNCTION mysql_get_socket

      ! int STDCALL mysql_real_query_start(int *ret, MYSQL *mysql, const char *q, unsigned long length)
      FUNCTION mysql_real_query_start(ret,mysql_, q, length ) bind(c,name = "mysql_real_query_start")
            import :: c_unsigned_long , MYSQL,c_int,c_char
            implicit none
            INTEGER(c_int)                      :: ret  
            type(MYSQL)                         :: mysql_
            CHARACTER(C_CHAR),DIMENSION(*)      :: q
            INTEGER(c_unsigned_long),VALUE      :: length
            
            !GCC$ ATTRIBUTES STDCALL            :: mysql_real_query_start   
            INTEGER(c_int)                      :: mysql_real_query_start
      END FUNCTION mysql_real_query_start

      ! int STDCALL mysql_real_query_cont(int *ret, MYSQL *mysql, int status)
      FUNCTION mysql_real_query_cont(ret,mysql_, status) bind(c,name = "mysql_real_query_cont")
            import ::  MYSQL,c_int
            implicit none
            INTEGER(c_int)                      :: ret  
            type(MYSQL)                         :: mysql_
            INTEGER(c_int),VALUE                :: status
            
            !GCC$ ATTRIBUTES STDCALL            :: mysql_real_query_cont   
            INTEGER(c_int)                      :: mysql_real_query_cont
      END FUNCTION mysql_real_query_cont

      ! MYSQL_RES  * STDCALL mysql_use_result(MYSQL *mysql);
      FUNCTION mysql_use_result(mysql_) bind(c,name = "mysql_use_result")
            import :: c_ptr , MYSQL
            implicit none
            type(MYSQL)                         :: mysql_

            !GCC$ ATTRIBUTES STDCALL            :: mysql_use_result   
            type(c_ptr)                         :: mysql_use_result
      END FUNCTION mysql_use_result

      ! void STDCALL mysql_free_result(MYSQL_RES *result)
      subroutine mysql_free_result(result) bind(c,name = "mysql_free_result")
            import :: MYSQL_RES
            implicit none
            type(MYSQL_RES)                     :: result
            !GCC$ ATTRIBUTES STDCALL            :: mysql_free_result   
      end subroutine mysql_free_result

      ! int STDCALL mysql_fetch_row_start(MYSQL_ROW *ret,MYSQL_RES *result)
      FUNCTION mysql_fetch_row_start(ret,result)  bind(c,name = "mysql_fetch_row_start")
            import :: c_ptr , MYSQL_RES , c_int
            implicit none

            type(c_ptr)                                      :: ret
            type(MYSQL_RES)                                  :: result 
            !GCC$ ATTRIBUTES STDCALL                         :: mysql_fetch_row_start   
            INTEGER(c_int)                                   :: mysql_fetch_row_start
      END FUNCTION mysql_fetch_row_start


      !unsigned int STDCALL mysql_errno(MYSQL *mysql)
      FUNCTION mysql_errno(mysql_) bind(c,name = "mysql_errno")
            import ::  MYSQL,DWORD
            implicit none

            type(MYSQL)                         :: mysql_
            !GCC$ ATTRIBUTES STDCALL            :: mysql_errno
            INTEGER(DWORD)                      :: mysql_errno
      END FUNCTION mysql_errno


      ! int STDCALL mysql_fetch_row_cont(MYSQL_ROW *ret, MYSQL_RES *result,int status)
      FUNCTION mysql_fetch_row_cont(ret,result,status)  bind(c,name = "mysql_fetch_row_cont")
            import :: c_ptr , MYSQL_RES , c_int
            implicit none

            type(c_ptr)                                      :: ret
            type(MYSQL_RES)                                  :: result 
            INTEGER(c_int),VALUE                             :: status

            !GCC$ ATTRIBUTES STDCALL                         :: mysql_fetch_row_cont   
            INTEGER(c_int)                                   :: mysql_fetch_row_cont
      END FUNCTION mysql_fetch_row_cont

      subroutine mysql_server_end() bind(c,name = "mysql_server_end")
            implicit none

      end subroutine mysql_server_end

!       int WSAAPI WSAPoll(
!   [in, out] LPWSAPOLLFD fdArray,
!   [in]      ULONG       fds,
!   [in]      INT         timeout
      FUNCTION WSAPoll(fdArray,fds,timeout)  bind(c,name = "WSAPoll")
            import :: pollfd,c_unsigned_long, c_int
            implicit none

            type(pollfd)                       :: fdArray
            INTEGER(c_unsigned_long),VALUE     :: fds 
            INTEGER(c_int),VALUE               :: timeout

            !GCC$ ATTRIBUTES STDCALL            :: WSAPoll   
            INTEGER(c_int)                      :: WSAPoll
      END FUNCTION WSAPoll


      ! unsigned int STDCALL mysql_get_timeout_value(const MYSQL *mysql)

      FUNCTION mysql_get_timeout_value(mysql_) bind(c,name = "mysql_get_timeout_value")
            import :: DWORD , MYSQL
            implicit none
            type(MYSQL)                         :: mysql_
            !GCC$ ATTRIBUTES STDCALL            :: mysql_get_timeout_value   
            INTEGER(DWORD)                      :: mysql_get_timeout_value
      END FUNCTION mysql_get_timeout_value

      ! ! unsigned int STDCALL mysql_num_fields(MYSQL_RES *res)
      FUNCTION mysql_num_fields(res) bind(c,name = "mysql_num_fields")
            import :: MYSQL_RES , c_int
            implicit none
            type(MYSQL_RES)                     :: res
            !GCC$ ATTRIBUTES STDCALL            :: mysql_num_fields   
            INTEGER(c_int)                      :: mysql_num_fields
      END FUNCTION mysql_num_fields
      !unsigned long * STDCALL mysql_fetch_lengths(MYSQL_RES *result)
      FUNCTION mysql_fetch_lengths(res) bind(c,name = "mysql_fetch_lengths")
            import :: MYSQL_RES , c_ptr
            implicit none
            type(MYSQL_RES)                     :: res
            !GCC$ ATTRIBUTES STDCALL            :: mysql_fetch_lengths   
            type(c_ptr)                         :: mysql_fetch_lengths
      END FUNCTION mysql_fetch_lengths

      ! unsigned long long STDCALL mysql_num_rows(MYSQL_RES *res)
      FUNCTION mysql_num_rows(res) bind(c,name = "mysql_num_rows")
            import :: c_ptr ,c_uint64_t
            implicit none
            type(c_ptr),VALUE                   :: res
            !GCC$ ATTRIBUTES STDCALL            :: mysql_num_rows   
            INTEGER(c_uint64_t)                 :: mysql_num_rows
      END FUNCTION mysql_num_rows

      ! MYSQL_RES *	STDCALL mysql_store_result(MYSQL *mysql)
      FUNCTION mysql_store_result(mysql_) bind(c,name = "mysql_store_result")
            import :: c_ptr , MYSQL
            implicit none
            type(MYSQL)                         :: mysql_
            !GCC$ ATTRIBUTES STDCALL            :: mysql_store_result   
            type(c_ptr)                         :: mysql_store_result
      END FUNCTION mysql_store_result

      END INTERFACE

      INTERFACE mysql_library_end
            procedure :: mysql_server_end
      END INTERFACE mysql_library_end

      contains

      FUNCTION DATABASE(IP_ADDRESS,USERNAME,PASSWORD,PORT_ID)
            implicit none
            TYPE(DATABASE_CONNECT_INFO)                      :: DATABASE
            CHARACTER(len = *)                               :: IP_ADDRESS
            CHARACTER(len = *)                               :: USERNAME
            CHARACTER(len = *)                               :: PASSWORD 
            INTEGER                                          :: PORT_ID 

            DATABASE = DATABASE_CONNECT_INFO(IP_ADDRESS,USERNAME,PASSWORD,PORT_ID)
            ALLOCATE(DATABASE%DATA(0,0))
      END FUNCTION

      FUNCTION wait_for_mysql(mysql_, status) 
            implicit none
            
            type(MYSQL)                   :: mysql_
            INTEGER                       :: status
            type(pollfd)                  :: pfd
            INTEGER                       :: timeout, res
            INTEGER                       :: wait_for_mysql

            pfd = pollfd(0,0,0)
            pfd%fd = mysql_get_socket(mysql_)
            ! print *, "pfd%fd = ", pfd%fd

            if (status == MYSQL_WAIT_READ) then 
                  ! print *, "pfd%events = POLLIN"
                  pfd%events = POLLIN
            else if (status == MYSQL_WAIT_WRITE) then 
                  ! print *, "pfd%events = POLLOUT"
                  pfd%events = POLLOUT
            else if (status == MYSQL_WAIT_EXCEPT) then 
                  ! print *, "pfd%events = POLLPRI"
                  pfd%events = POLLPRI
            else 
                  ! print *, "pfd%events = 0"
                  pfd%events = 0
            end if

            if (status == MYSQL_WAIT_TIMEOUT) then 
              timeout = 1000*mysql_get_timeout_value(mysql_)
            else
              timeout = 3000
            end if 
            res = WSAPoll(pfd, 1, timeout)
            if (res == 0) then
               wait_for_mysql = MYSQL_WAIT_TIMEOUT
            else if (res < 0) then 
               wait_for_mysql = MYSQL_WAIT_TIMEOUT
            else 
              wait_for_mysql = 0
              if (pfd%revents == POLLIN)  wait_for_mysql = IOR(status,MYSQL_WAIT_READ)
              if (pfd%revents == POLLOUT) wait_for_mysql = IOR(status,MYSQL_WAIT_WRITE) 
              if (pfd%revents == POLLPRI) wait_for_mysql = IOR(status,MYSQL_WAIT_EXCEPT) 
            end if 
      END FUNCTION  wait_for_mysql

      SUBROUTINE EXECUTE_COMMAND_DATABASE(comm,ipaddress,username,password,port_id,DATA_)
            type(c_ptr)                                       :: mysql_ptr
            type(c_ptr)                                       :: row
            type(c_ptr),DIMENSION(:),POINTER                  :: fields_ptr_array
      
            CHARACTER(kind = c_char),DIMENSION(:),POINTER     :: row_variables
            CHARACTER(len = 1000)                             :: row_variables_f
            type(db_field_in_row),DIMENSION(:),ALLOCATABLE    :: field_value
      
            type(MYSQL),POINTER                               :: mysql_variable
            INTEGER                                           :: status, err 
            type(MYSQL_RES),POINTER                           :: res
            type(c_ptr)                                       :: res_ptr
            INTEGER                                           :: number_fields
            INTEGER(c_uint64_t)                               :: number_rows
            integer(c_unsigned_long),DIMENSION(:),POINTER     :: stringlen
            type(c_ptr)                                       :: field_length_array
            INTEGER                                           :: i, rsv,a 
            CHARACTER(len = *)                                :: comm
            CHARACTER(len = *)                                :: ipaddress
            CHARACTER(len = *)                                :: username
            CHARACTER(len = *),OPTIONAL                       :: password

            CHARACTER(c_char),DIMENSION(260)                  :: comm_
            CHARACTER(c_char),DIMENSION(260)                  :: ipaddress_
            CHARACTER(c_char),DIMENSION(260)                  :: username_
            CHARACTER(c_char),DIMENSION(260)                  :: password_

            INTEGER                                           :: port_id

            TYPE(db_field_in_row),DIMENSION(:,:),ALLOCATABLE,OPTIONAL     :: DATA_ 

            comm_ = cchar(comm)
            ipaddress_ = cchar(ipaddress)
            username_ = cchar(username)
            
            rsv = mysql_library_init(0,c_null_ptr,c_null_ptr)    
            mysql_ptr = mysql_init()
            CALL C_F_POINTER(mysql_ptr, mysql_variable)
      
            rsv = mysql_options(mysql_variable, MYSQL_OPT_NONBLOCK, c_null_ptr)
            
            if (.NOT.PRESENT(password)) then 
                  status = mysql_real_connect_start(mysql_ptr,mysql_variable,ipaddress_, &
                  user = username_, port = port_id, clientflag = 0)
            else 
                  password_ = cchar(password)
                  status = mysql_real_connect_start(mysql_ptr,mysql_variable,ipaddress_, &
                  user = username_,password = password_, port = port_id, clientflag = 0)
            end if 

            do while (status /=  0)        
              rsv = wait_for_mysql(mysql_variable,status)
              status = mysql_real_connect_cont(mysql_ptr,mysql_variable,status)
            end do 

            status = mysql_real_query_start(err, mysql_variable, comm_,len(comm))
            do while (status /= 0) 
              rsv = wait_for_mysql(mysql_variable,status)
              status = mysql_real_query_cont(err, mysql_variable, status)
            end do 

            res_ptr = mysql_store_result(mysql_variable)
            ! res_ptr = mysql_use_result(mysql_variable)

            if ((.NOT.c_associated(res_ptr)).OR.(c_associated(res_ptr,c_null_ptr))) then 
                  print *, "mysql_use_result() returns error"
                  res_ptr = c_null_ptr
                  CALL mysql_close(mysql_variable)
                  mysql_ptr = c_null_ptr
                  call mysql_library_end
                  RETURN
            end if 

            number_rows = mysql_num_rows(res_ptr)
            call C_F_POINTER(res_ptr, res)

            number_fields = mysql_num_fields(res)

            ALLOCATE(field_value(number_fields))
            if (PRESENT(DATA_)) then 
                  if ((SIZE(DATA_,DIM = 1) /= number_rows).OR.(SIZE(DATA_,DIM = 2) /= number_fields)) then 
                        if (ALLOCATED(DATA_)) DEALLOCATE(DATA_)
                        ALLOCATE(DATA_(number_rows,number_fields))
                  end if 
            end if 
            a = 0
            do 
              a = a + 1 
              status = mysql_fetch_row_start(row, res)
              do while (status /= 0) 
                rsv = wait_for_mysql(mysql_variable,status)
                status = mysql_fetch_row_cont(row, res, status)
              end do 
      
              if (.not.c_associated(row)) exit 
              call C_F_POINTER(row,fields_ptr_array,[number_fields])
      
              field_length_array = mysql_fetch_lengths(res)
        
              call C_F_POINTER(field_length_array,stringlen,[number_fields])
      
              do i = 1 , number_fields
                call C_F_POINTER(fields_ptr_array(i),row_variables,[stringlen(i)])
                call c_f_str_chars_mariadblib(row_variables,row_variables_f)
                field_value(i)%text = TRIM(ADJUSTL(row_variables_f))
                if (PRESENT(DATA_)) then 
                  DATA_(a,i)%text = TRIM(ADJUSTL(row_variables_f))
                end if 
              end do 
            WRITE(*,'(*(A50,5x))') (field_value(i)%text, i = 1,number_fields)
            end do 
      
            if (mysql_errno(mysql_variable) /= 0) print *, "Got error while retrieving rows"
            call mysql_free_result(res)
            res_ptr = c_null_ptr
            CALL mysql_close(mysql_variable)
            mysql_ptr = c_null_ptr
            call mysql_library_end
      END SUBROUTINE EXECUTE_COMMAND_DATABASE

      FUNCTION cchar(fstr)
            implicit none
            CHARACTER(len = *)                            ::  fstr 
            CHARACTER(c_char),DIMENSION(len(fstr)+1)      ::  cchar
            INTEGER                                       ::  i
            
            do i = 1, len(fstr)+1
                  if (i <= len(fstr)) then 
                        cchar(i) = fstr(i:i)
                  else 
                        cchar(i) = c_null_char
                  end if 
            end do 
      END FUNCTION cchar

      SUBROUTINE RUN(this,comm)
            implicit none
            CLASS(DATABASE_CONNECT_INFO)        :: this
            CHARACTER(len = *)                  :: comm
            ! INTEGER                             :: a, i
            WRITE(*,*) comm
            if ((this%PASSWORD /= "").AND.(ALLOCATED(this%PASSWORD))) then 
                  CALL EXECUTE_COMMAND_DATABASE(comm,this%IP_ADDRESS,&
                  this%USERNAME,this%PASSWORD,this%PORT_ID,this%DATA)
            else 
                  CALL EXECUTE_COMMAND_DATABASE(comm,this%IP_ADDRESS,&
                  this%USERNAME,port_id = this%PORT_ID,DATA_=this%DATA)
            end if 

            ! do a = 1,size(this%DATA,1)
            !       WRITE(*,'(*(A50,5x))') (this%DATA(a,i)%text, i = 1,size(this%DATA,2)) 
            ! end do 
      END SUBROUTINE RUN

      subroutine c_f_str_chars_mariadblib(c_str, f_str)
            CHARACTER(len=1, kind=c_char), intent(in)  :: c_str(*)
            CHARACTER(len=*),              intent(out) :: f_str
            INTEGER                                    :: i
    
            i = 1
    
            do while (c_str(i) /= c_null_char .and. i <= len(f_str))
                f_str(i:i) = c_str(i)
                i = i + 1
            end do
    
            if (i < len(f_str)) f_str(i:) = ' '
      end subroutine c_f_str_chars_mariadblib
 end module mariadb_fortran