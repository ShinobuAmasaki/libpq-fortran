module m_fe_connect
   implicit none
   private

   public :: PQconnectdb
   public :: PQconnectdbParams
   public :: PQdb
   public :: PQhost
   public :: PQping
   public :: PQuser
   public :: PQhostaddr
   public :: PQstatus
   public :: PQfinish
   public :: PQerrorMessage
   public :: PQoptions
   public :: PQtransactionStatus
   public :: PQsetdbLogin
   public :: PQpingParams
   public :: PQreset

   ! Deprecated functions
   ! - PQtty
   ! - PQrequestCancel

contains

   !==================================================================!
   ! Database Connection Control Functions

   function PQconnectdb(conninfo) result(conn)
      use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_null_char
      implicit none
      
      character(*), intent(in) :: conninfo
      character(:, kind=c_char), allocatable :: c_conninfo

      type(c_ptr) :: conn

      interface
         ! Interface to PQconnectdb in interface/libpq/fe-connect.c:
         !
         ! PGconn *PQconnectdb(const char *conninfo)

         function c_PQ_connectdb(info) bind(c, name="PQconnectdb") result(conn)
            import c_ptr, c_char
            implicit none
            character(1, kind=c_char), intent(in) :: info(*)
            type(c_ptr) :: conn
         end function
      end interface 

      c_conninfo = conninfo//c_null_char

      conn = c_PQ_connectdb(c_conninfo)
      
   end function PQconnectdb


   function PQconnectdbParams (keywords, values, expand_dbname) result(conn)
      use :: character_operations
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char,&
                                             c_null_char, c_null_ptr, c_loc
      implicit none

      ! NULL終端ではない配列を受け取る
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)

      integer :: expand_dbname
      type(c_ptr) :: conn

      integer :: max_len_key, max_len_val

      interface
         function c_PQ_connectdb_params (keywords, values, expand_dbname) &
                                           bind(c, name="PQconnectdbParams") result(conn)
            import c_ptr, c_int
            implicit none
            ! ポインタの配列を渡すのでvalue属性は付けない。
            type(c_ptr), intent(in) :: keywords ! an array of pointers
            type(c_ptr), intent(in) :: values   ! an array of pointers
            integer(c_int), intent(in) :: expand_dbname
            type(c_ptr) :: conn
         end function c_PQ_connectdb_params
      end interface


      ! 下で確保する文字列配列の長さを知る。
      max_len_key = max_length_char_array(keywords)
      max_len_val = max_length_char_array(values)

      block
         ! null文字をつけるために最大値よりも1だけ大きい文字列を宣言する。
         character(max_len_key+1, kind=c_char), allocatable, target ::  c_keys(:)
         character(max_len_val+1, kind=c_char), allocatable, target ::  c_values(:)

         ! ポインタの配列を宣言する。
         type(c_ptr), allocatable :: ptr_keys(:)
         type(c_ptr), allocatable :: ptr_values(:)

         ! c_int型整数を宣言する。
         integer(c_int) :: c_expand_dbname

         ! keywordsについて、Cに渡す文字列の配列を用意する。
         call cchar_array_from_strings(keywords, c_keys, max_len_key)
         ! keywordsについて、Cに渡すポインタの配列を用意する。
         call cptr_array_from_cchar(c_keys, ptr_keys)

         ! valuesについて、Cに渡す文字列の配列を用意する。
         call cchar_array_from_strings(values, c_values, max_len_val)
         ! valuesについて、Cに渡すポインタの配列を用意する。
         call cptr_array_from_cchar(c_values, ptr_values)

         c_expand_dbname = expand_dbname

         conn = c_PQ_connectdb_params(ptr_keys, ptr_values, c_expand_dbname)

         end block

   end function PQconnectdbParams


   function PQsetdbLogin (host, port, options, tty, dbName, login, pwd) result(conn)
      use, intrinsic :: iso_c_binding
      implicit none
      character(*), intent(in) :: host
      character(*), intent(in) :: port
      character(*), intent(in) :: options
      character(*), intent(in) :: tty
      character(*), intent(in) :: dbName
      character(*), intent(in) :: login
      character(*), intent(in) :: pwd
      
      type(c_ptr) :: conn

      interface
         function c_PQ_setdb_login (pghost, pgport, pgoptions, pgtty, dbName, login, pwd) &
               bind(c, name='PQsetdbLogin') result(res)
            import c_ptr, c_char
            implicit none
            character(1, kind=c_char), intent(in) :: pghost(*)
            character(1, kind=c_char), intent(in) :: pgport(*)
            character(1, kind=c_char), intent(in) :: pgoptions(*)
            character(1, kind=c_char), intent(in) :: pgtty(*)
            character(1, kind=c_char), intent(in) :: dbName(*)
            character(1, kind=c_char), intent(in) :: login(*)
            character(1, kind=c_char), intent(in) :: pwd(*)
            type(c_ptr) :: res
         end function c_PQ_setdb_login
      end interface

      block
         character(:, kind=c_char), allocatable :: c_host, c_port, &
                                       c_options, c_tty, c_dbName, &
                                       c_login, c_pwd
         
         ! Cの関数にわたす文字列の末尾にNULL文字をつける。
         c_host      = host//c_null_char
         c_port      = port//c_null_char
         c_options   = options//c_null_char
         c_tty       = tty//c_null_char
         c_dbName    = dbName//c_null_char
         c_login     = login//c_null_char
         c_pwd       = pwd//c_null_char

         conn = c_PQ_setdb_login(c_host, c_port, c_options, c_tty, c_dbName, c_login, c_pwd)
      end block

   end function PQsetdbLogin


   ! function PQconnectStartParams
   ! function PQconnectStart
   ! function PQconnectPoll
   ! subroutine PQconninfodefaults
   ! function PQconninfo
   ! function PQconninfoParse


   !
   subroutine PQconninfoFree (connopts)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: connopts

      interface
         subroutine c_PQ_conninfo_free(connOptions) bind(c, name="PQconninfoFree")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: connOptions
         end subroutine c_PQ_conninfo_free
      end interface

      call c_PQ_conninfo_free(connopts)
   end subroutine PQconninfoFree


   subroutine PQfinish(conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn

      interface
         ! Interface ot PQfinish in interfaces/libpq/fe-connect.c:
         ! 
         ! void PQfinish(PGconn *conn)

         subroutine c_PQ_finish(conn) bind(c, name='PQfinish')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
         end subroutine c_PQ_finish
      end interface

      call c_PQ_finish(conn)
   end subroutine PQfinish


   subroutine PQreset (conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(inout) :: conn

      interface
         subroutine c_PQ_reset(conn) bind(c, name='PQreset')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
         end subroutine c_PQ_reset
      end interface

      call c_PQ_reset(conn)

   end subroutine PQreset
            

   ! function PQresetStart
   ! function PQresetPoll


   function PQpingParams (keywords, values, expand_dbname) result(res)
      use :: character_operations
      use, intrinsic :: iso_c_binding
      implicit none
      
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)
      integer, intent(in) :: expand_dbname
      integer(c_int) :: res
      
      integer :: max_len_key, max_len_val

      interface
         function c_PQ_ping_params (keywords, values, expand_dbname) &
                                       bind(c, name="PQpingParams")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in) :: keywords
            type(c_ptr), intent(in) :: values
            integer(c_int), intent(in) :: expand_dbname
            integer(c_int) :: c_PQ_ping_params
         end function c_PQ_ping_params
      end interface

      max_len_key = max_length_char_array(keywords)
      max_len_val = max_length_char_array(values)

      block
         character(max_len_key+1, kind=c_char), allocatable, target :: c_keys(:)
         character(max_len_val+1, kind=c_char), allocatable, target :: c_values(:)

         type(c_ptr), allocatable :: ptr_keys(:)
         type(c_ptr), allocatable :: ptr_values(:)

         integer(c_int) :: c_expand_dbname

         call cchar_array_from_strings(keywords, c_keys, max_len_key)
         call cptr_array_from_cchar(c_keys, ptr_keys)

         call cchar_array_from_strings(values, c_values, max_len_val)
         call cptr_array_from_cchar(c_values, ptr_values)

         c_expand_dbname = expand_dbname

         res = c_PQ_ping_params(ptr_keys, ptr_values, c_expand_dbname)
      end block

   end function PQpingParams
         
      
   function PQping(conninfo) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: conninfo
      character(:, kind=c_char), allocatable :: c_conninfo
      integer(int32) :: res

      interface 
         function c_PQ_ping (info) bind(c, name="PQping") result(c_res)
            import c_char, c_int
            implicit none
            character(1, kind=c_char), intent(in) :: info(*)
            integer(c_int) :: c_res
         end function c_PQ_ping
      end interface

      c_conninfo = conninfo//c_null_char

      res = c_PQ_ping(c_conninfo)

   end function PQping


   ! function PQsetSSLKeyPassHook_OpenSSL
   ! function PQgetSSLKeyPassHook_OpenSSL
   

   !==================================================================!
   ! Connection Status Functions


   function PQdb (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQdb in src/interfaces/fe-connection.c
      interface
         function  c_PQ_db(conn) bind(c, name="PQdb")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_db
         end function c_PQ_db
      end interface

      res => c_to_f_charpointer(c_PQ_db(conn))

   end function PQdb


   function PQuser (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQuserin src/interfaces/fe-connection.c
      interface
         function  c_PQ_user(conn) bind(c, name="PQuser")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_user
         end function c_PQ_user
      end interface

      res => c_to_f_charpointer(c_PQ_user(conn))

   end function PQuser


   ! function PQpass


   function PQhost (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQhost in src/interfaces/fe-connection.c
      interface
         function  c_PQ_host(conn) bind(c, name="PQhost")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_host
         end function c_PQ_host
      end interface

      res => c_to_f_charpointer(c_PQ_host(conn))

   end function PQhost


   function PQhostaddr (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQhostaddr in src/interfaces/fe-connection.c
      interface
         function  c_PQ_hostaddr(conn) bind(c, name="PQhostaddr")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_hostaddr
         end function c_PQ_hostaddr
      end interface

      res => c_to_f_charpointer(c_PQ_hostaddr(conn))

   end function PQhostaddr

   
   ! function PQport


   function PQoptions (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQoptions in src/interface/fe-connection.c
      interface
         function c_PQ_options (conn) bind(c, name="PQoptions")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_options
         end function c_PQ_options
      end interface

      res => c_to_f_charpointer(c_PQ_options(conn))
   
   end function PQoptions
      

   function PQstatus(conn) result(res)
      use, intrinsic :: iso_fortran_env, only: int32
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res 

      interface
         ! Interface to PQstatus in interface/libpq/fe-connect.c:
         ! 
         ! ConnStatusType PQstatus(const PGconn *conn)

         function c_PQ_status(conn) bind(c, name='PQstatus') result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: res
         end function c_PQ_status
      end interface

      res = c_PQ_status(conn)

   end function PQstatus


   function PQtransactionStatus (conn) result (res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      integer(c_int) :: res

      interface
         function c_PQ_transaction_status (conn)  &
               bind(c, name="PQtransactionStatus") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: res
         end function c_PQ_transaction_status
      end interface

      res = c_PQ_transaction_status(conn)
   end function PQtransactionStatus


   ! function PQparameterStatus
   ! function PQprotocolVersion
   ! function PQserverVersion


   function PQerrorMessage(conn)
      use ::  character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(:, c_char), pointer :: PQerrormessage

      interface
         ! Interface to PQerrorMessage in interfaces/libpq/fe-connect.c:
         !
         ! char *PQerrorMessage(const PGconn *conn)

         function c_PQ_error_message(conn) bind(c, name='PQerrorMessage')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_error_message
         end function c_PQ_error_message
      end interface

      PQerrormessage => c_to_f_charpointer(c_PQ_error_message(conn))
   end function PQerrorMessage


   ! function PQsocket
   ! function PQbackendPID
   ! function PQconnectionNeedsPassword
   ! function PQconnectionUsedPassword

   != for SSL connection
   ! function PQsslInUse
   ! function PQsslAttribute
   ! function PQsslAttributeNames
   ! function PQsslStruct
   ! function PQgetssl


   !==================================================================!
   ! Canceling Queries in Progress

   ! funciton PQgetCancel
   ! function PQfreeCancel
   ! funciton PQcancel


   !==================================================================!
   ! Control Functions

   ! function PQclientEncoding
   ! function PQsetClientEncoding
   ! function PQsetErrorVerbosity
   ! function PQsetErrorContextVisibility
   

end module m_fe_connect