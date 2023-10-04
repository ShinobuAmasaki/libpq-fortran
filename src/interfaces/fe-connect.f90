module fe_connect_m
   implicit none
   private

   public :: PQconnectdb
   public :: PQconnectdbParams
   public :: PQdb
   public :: PQhost
   public :: PQping
   public :: PQuser
   public :: PQpass
   public :: PQport
   public :: PQhostaddr
   public :: PQstatus
   public :: PQfinish
   public :: PQerrorMessage
   public :: PQoptions
   public :: PQtransactionStatus
   public :: PQsetdbLogin
   public :: PQpingParams
   public :: PQreset
   public :: PQserverVersion
   public :: PQprotocolVersion
   public :: PQconndefaults
   public :: PQconnectStart
   public :: PQconnectStartParams
   public :: PQconnectPoll
   public :: PQsocket
   public :: PQbackendPID
   public :: PQresetPoll
   public :: PQresetStart
   public :: PQparameterStatus
   public :: PQconnectionNeedsPassword
   public :: PQconnectionUsedPassword
   public :: PQconninfo
   public :: PQconninfoParse

   public :: PQclientEncoding
   public :: PQsetClientEncoding
   public :: PQsslInUse
   public :: PQsslAttribute
   public :: PQsslAttributeNames
   
   public :: PQgetCancel
   public :: PQfreeCancel
   public :: PQcancel

   public :: PQsetErrorVerbosity
   public :: PQsetErrorContextVisibility

   ! PRIVATE functions
   private :: PQconnectdbParams_back


contains

   !==================================================================!
   ! Database Connection Control Functions

   !> Connect to the database server with the provided connection string `conninfo`.
   function PQconnectdb(conninfo) result(conn)
      use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_null_char
      implicit none

      !> PostgreSQL connection string
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

      !*### Example
      !```Fortran
      !   character(:), allocatable :: conninfo
      !   type(c_ptr) :: conn
      ! 
      !   conninfo = "host=localhost user=postgres dbname=postgres password=foobar" 
      !
      !   conn = PQcoonectdb(conninfo)
      !
      !   ! Error handling
      !   if (PQstatus(conn) /= CONNECTION_OK) then
      !      print *, PQerrorMessage(conn)
      !   end if
      !```
      

      
   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTDB)
   end function PQconnectdb


   !> Connect to the database server with connection infomation provided as pairs of `keywords` and `values` arrays.
   function PQconnectdbParams (keywords, values, expand_dbname) result(conn)
      use, intrinsic :: iso_c_binding

      !> Keywords, such as `host`, `hostaddr`, `dbname`, `user`, `password`, etc.
      character(*), intent(in) :: keywords(:)

      !> The array of each value corresponding to that keyword.
      character(*), intent(in) :: values(:)

      integer :: expand_dbname
      type(c_ptr) :: conn

      conn =  PQconnectdbParams_back(keywords, values, expand_dbname, .false.)

      !*### Example
      !```Fortran
      !   character(16) :: keywords(3), values(3)
      !   type(c_ptr) :: conn
      !   
      !   keywords(1) = 'host';   values(1) = 'localhost'
      !   keywords(2) = 'user';   values(2) = 'postgres'
      !   keywords(3) = 'dbname'; values(3) = 'postgres'
      !   
      !   conn = PQconnectdbParams(keywords, values, 0)
      !   
      !   ! Error handling
      !   if (PQstatus(conn) /= CONNECTION_OK) then
      !      print *, PQerrorMessage(conn)
      !   end if
      !```

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTDBPARAMS)
   end function PQconnectdbParams
      

   function PQconnectdbParams_back (keywords, values, expand_dbname, isNonblocking) result(conn)
      use :: character_operations_m
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char,&
                                             c_null_char, c_null_ptr, c_loc, c_associated
      implicit none

      ! NULL終端ではない配列を受け取る
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)

      integer :: expand_dbname
      logical :: isNonblocking

      type(c_ptr) :: conn

      integer :: max_len_key, max_len_val

      interface
         function c_PQ_connectdb_params (keywords, values, expand_dbname) &
                                           bind(c, name="PQconnectdbParams") result(conn)
            import c_ptr, c_int
            implicit none
            ! ポインタの配列を渡すのでvalue属性は付けない。
            type(c_ptr), intent(in), value :: keywords! an array of pointers
            type(c_ptr), intent(in), value :: values  ! an array of pointers
            integer(c_int), intent(in) :: expand_dbname
            type(c_ptr) :: conn
         end function c_PQ_connectdb_params
      end interface

      interface
         function c_PQ_connect_start_params (keywords, values, expand_dbname) &
                                             bind(c, name="PQconnectStartParams") result(conn)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: keywords
            type(c_ptr), intent(in), value :: values
            integer(c_int), intent(in) :: expand_dbname
            type(c_ptr) :: conn
         end function c_PQ_connect_start_params
      end interface

      ! 下で確保する文字列配列の長さを知る。
      max_len_key = max_length_char_array(keywords)
      max_len_val = max_length_char_array(values)

      block
         ! null文字をつけるために最大値よりも1だけ大きい文字列を宣言する。
         character(max_len_key+1, kind=c_char), allocatable, target ::  c_keys(:)
         character(max_len_val+1, kind=c_char), allocatable, target ::  c_values(:)

         ! ポインタの配列を宣言する。
         type(c_ptr), allocatable, target :: ptr_keys(:)
         type(c_ptr), allocatable, target :: ptr_values(:)

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

         ! ノンブロッキング接続かどうかで分岐する。
         if (isNonblocking) then
            conn = c_PQ_connect_start_params(c_loc(ptr_keys(1)), c_loc(ptr_values(1)), c_expand_dbname)
         else
            conn = c_PQ_connectdb_params(c_loc(ptr_keys(1)), c_loc(ptr_values(1)), c_expand_dbname)
         end if

      end block

   end function PQconnectdbParams_back


   !> Connect to the database server with connection infomation provided as each argument
   function PQsetdbLogin (host, port, options, tty, dbName, login, pwd) result(conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      !> Hostname 
      character(*), intent(in) :: host
      !> Port number
      character(*), intent(in) :: port
      !> Options
      character(*), intent(in) :: options
      !> Empty string
      character(*), intent(in) :: tty
      !> The name of the database
      character(*), intent(in) :: dbName
      !> The user name at this login
      character(*), intent(in) :: login
      !> The password of the user
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

      !*### Example
      !```Fortran
      !   type(c_ptr) :: conn
      !   
      !   conn = PQsetdbLogin("localhost", "5432", "", "", "postgres","postgres", "")
      !
      !   ! Error handling
      !   if (PQstatus(conn) /= CONNECTION_OK) then
      !      print *, PQerrorMessage(conn)
      !   end if
      !```


   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQSETDBLOGIN)
   end function PQsetdbLogin

   
   !> Connect to the database server in a nonblocking manner.
   function PQconnectStartParams(keywords, values, expand_dbname) result(conn)
      use, intrinsic :: iso_c_binding
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)
      integer :: expand_dbname
      type(c_ptr) :: conn

      conn =  PQconnectdbParams_back(keywords, values, expand_dbname, .true.)

      !*### Example
      !```Fortran
      !   type(c_ptr) :: conn
      !   integer :: res = -1
      !   character(16) :: keywords(3), values(3)
      !   type(c_ptr) :: conn
      !   
      !   keywords(1) = 'host';   values(1) = 'localhost'
      !   keywords(2) = 'user';   values(2) = 'postgres'
      !   keywords(3) = 'dbname'; values(3) = 'postgres'   
      !   
      !   conn = PQconnectStartParams(keywords, values, 0)
      !
      !   if (c_associated(conn)) then
      !      do while(res /= PGRES_POLLING_OK) ! loop for polling
      !         
      !         res = PQconnectPoll(conn)
      !         select case (res)
      !         case (PGRES_POLLING_FAILED)
      !            print *, PQerrorMessage(conn)
      !            error stop
      !         case (PGRES_POLLING_OK)
      !            print *, "CONNECTION ESTABLISHED"
      !            exit
      !         case default
      !            continue   
      !            ! write some process here! 
      !         end select
      ! 
      !      end do
      !   else
      !      print *, "Cannot connect the server."
      !      error stop 
      !   end if
      !```

      !*### References
      ! cf. [[PQconnectStart]], [[PQconnectPoll]]

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTSTARTPARAMS)
   end function PQconnectStartParams


   !> Connect to the database server in a nonblocking manner.
   function PQconnectStart (conninfo) result(conn)
      use, intrinsic :: iso_c_binding, only:c_char, c_ptr, c_null_char
      implicit none
      
      character(*), intent(in) :: conninfo
      character(:, kind=c_char), allocatable :: c_conninfo

      type(c_ptr) :: conn

      interface
         function c_PQ_connect_start(info) bind(c, name="PQconnectStart") result(conn)
            import c_ptr, c_char
            implicit none
            character(1, kind=c_char), intent(in) :: info(*)
            type(c_ptr) :: conn
         end function c_PQ_connect_start
      end interface 

      c_conninfo = conninfo//c_null_char

      conn = c_PQ_connect_start(c_conninfo)

      !*### Example
      !```Fortran
      !   character(:), allocatable :: conninfo
      !   type(c_ptr) :: conn
      !   integer :: res = -1
      ! 
      !   conninfo = "host=localhost user=postgres dbname=postgres"
      !   
      !   conn = PQconnectStart(conninfo)
      !
      !   if (c_associated(conn)) then
      !      do while(res /= PGRES_POLLING_OK) ! loop for polling
      !         
      !         res = PQconnectPoll(conn)
      !         select case (res)
      !         case (PGRES_POLLING_FAILED)
      !            print *, PQerrorMessage(conn)
      !            error stop
      !         case (PGRES_POLLING_OK)
      !            print *, "CONNECTION ESTABLISHED"
      !            exit
      !         case default
      !            continue   
      !            ! write some process here! 
      !         end select
      ! 
      !      end do
      !   else
      !      print *, "Cannot connect the server."
      !      error stop 
      !   end if
      !```

      !*### References
      ! cf. [[PQconnectPoll]], [[PQconnectStartParams]]

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTSTARTPARAMS)
   end function PQconnectStart

   
   !> Connect to the database server in a nonblocking manner.
   function PQconnectPoll(conn)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      type(c_ptr) :: conn
      integer(int32) :: PQconnectPoll

      interface
         function c_PQ_connect_poll(conn) bind(c, name="PQconnectPoll")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_connect_poll
         end function c_PQ_connect_poll
      end interface

      PQconnectPoll = c_PQ_connect_poll(conn)

      !*### References
      ! cf. [[PQconnectStart]], [[PQconnectStartParams]]

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTSTARTPARAMS)
   end function PQconnectPoll


   !> Get the default connection options.
   subroutine PQconndefaults (options)
      use :: PQconninfoOption_t
      use :: character_operations_m
      use, intrinsic :: iso_c_binding
      implicit none

      type(PQconninfoOption), dimension(:), allocatable, target, intent(out) :: options

      interface
         function c_PQ_conndefaults_prepare (optionsizes) bind(c, name="PQconndefaultsPrepare")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(out) :: optionsizes
            integer(c_int) :: c_PQ_conndefaults_prepare
         end function c_PQ_conndefaults_prepare
      end interface

      interface
         function c_PQ_conndefaults () bind(c, name="PQconndefaults")
            import c_ptr
            implicit none
            type(c_ptr) :: c_PQ_conndefaults
         end function
      end interface 

      interface
         subroutine c_PQ_conndefault_prepare_free (cptr) bind(c, name="PQconndefaultPrepareFree")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cptr
         end subroutine
      end interface

      type(c_ptr) :: cptr_siz, cptr_obj
      type(c_PQconnOptionSizes),dimension(:), pointer :: fptr
      type(c_PQconninfoOption), dimension(:), pointer :: opts_ptr

      integer :: length, i

      length = c_PQ_conndefaults_prepare(cptr_siz)
      
      call c_f_pointer(cptr_siz, fptr, shape=[length])

      cptr_obj = c_PQ_conndefaults()

      call c_f_pointer(cptr_obj, opts_ptr, shape=[length])

      allocate(options(length))

      do i = 1, length
         call read_option(fptr(i), opts_ptr(i), options(i))
      end do
      
      call c_PQ_conndefault_prepare_free(cptr_siz)
      call PQconninfoFree(cptr_obj)

      !*### Example
      !```Fortran
      !
      !   type(PQconninfoOption), allocatable, target :: options(:)
      !   integer :: i
      ! 
      !   call PQconndefaults(options)
      !   
      !   ! Print the entire default options separated by colons
      !   do i = 1, size(options)
      !      print '(12a, i0)', trim(options(i)%keyword)," : ", &
      !         trim(options(i)%envvar), " : ", &
      !         trim(options(i)%compiled), " : ", &
      !         trim(options(i)%val), " : ", &
      !         trim(options(i)%label), " : ", &
      !         trim(options(i)%dispchar), " : ", &
      !         options(i)%dispsize
      !   end do
      !```

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNDEFAULTS)
   end subroutine PQconndefaults


   subroutine PQconninfo (conn, options)
      use :: character_operations_m
      use :: PQconninfoOption_t
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in) :: conn
      type(PQconninfoOption), dimension(:), allocatable, target, intent(out) :: options

      interface
         function c_PQ_conninfo_prepare (conn, optionsizes) bind(c, name="PQconninfoPrepare") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(out) :: optionsizes
            integer(c_int) :: res
         end function c_PQ_conninfo_prepare
      end interface

      interface
         function c_PQ_conninfo (conn) bind(c, name="PQconninfo")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn 
            type(c_ptr) :: c_PQ_conninfo
         end function c_PQ_conninfo
      end interface

      interface
         subroutine c_PQ_conninfo_prepare_free (cptr) bind(c, name="PQconninfoPrepareFree")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cptr
         end subroutine
      end interface

      type(c_ptr) :: cptr_siz, cptr_obj
      type(c_PQconnOptionSizes), dimension(:), pointer :: fptr
      type(c_PQconninfoOption), dimension(:), pointer :: opts_ptr
      integer :: length, i

      length = c_PQ_conninfo_prepare(conn, cptr_siz)

      call c_f_pointer(cptr_siz, fptr, shape=[length])

      cptr_obj = c_PQ_conninfo(conn)

      call c_f_pointer(cptr_obj, opts_ptr, shape=[length])

      allocate(options(length))

      do i = 1, length
         call read_option(fptr(i), opts_ptr(i), options(i))
      end do

      call c_PQ_conninfo_prepare_free(cptr_siz)
      call PQconninfoFree(cptr_obj)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNINFO)
   end subroutine PQconninfo
            

   subroutine PQconninfoParse(conninfo, options, errmsg, errflag)
      use :: PQconninfoOption_t
      use :: character_operations_m
      use, intrinsic :: iso_c_binding
      implicit none
      
      character(*), intent(in) :: conninfo
      character(*), intent(out) :: errmsg
      character(:, kind=c_char), allocatable :: c_conninfo
      type(PQconninfoOption), dimension(:), allocatable, target, intent(out) :: options

      type(c_ptr) :: cptr_siz, cptr_obj, cptr_errmsg
      type(c_PQconnOptionSizes), dimension(:), pointer :: fptr
      type(c_PQconninfoOption), dimension(:), pointer :: opts_ptr
      logical :: errflag
      integer :: length, i, errlen

      interface
         function c_PQ_conninfo_parse_prepare (info, errmsg, errlen, optionsizes)  &
                                    bind(c, name="PQconninfoParsePrepare")
            import c_ptr, c_char, c_int
            implicit none
            character(1, kind=c_char), intent(in) :: info(*)
            type(c_ptr), intent(inout) :: errmsg
            integer(c_int), intent(out) :: errlen
            type(c_ptr), intent(out) :: optionsizes
            integer(c_int) :: c_PQ_conninfo_parse_prepare
         end function c_PQ_conninfo_parse_prepare
      end interface

      interface
         function c_PQ_conninfo_parse (info, errmsg) bind(c, name="PQconninfoParse")
            import c_ptr, c_char
            implicit none
            character(1, kind=c_char), intent(in) :: info(*)
            type(c_ptr) :: errmsg
            type(c_ptr) :: c_PQ_conninfo_parse
         end function c_PQ_conninfo_parse
      end interface

      interface
         subroutine c_PQ_conninfo_parse_free (cptr) bind(c, name="PQconninfoParsePrepareFree")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cptr
         end subroutine c_PQ_conninfo_parse_free
      end interface

      ! エラーフラグを初期化する。
      errflag = .false.
      errlen = 0

      ! C文字列へ変換してCの関数を呼び出す。
      c_conninfo = trim(conninfo)//c_null_char
      length = c_PQ_conninfo_parse_prepare(c_conninfo, cptr_errmsg, errlen, cptr_siz)
      
      !　エラーメッセージが返された場合 
      if (.not. c_associated(cptr_siz)) then
         block
            character(errlen), pointer :: fptr_errmsg
            call c_f_pointer(cptr_errmsg, fptr_errmsg)
            if (associated(fptr_errmsg)) then
               ! errlen-1は末尾の改行を含めないため。
               errmsg = fptr_errmsg(1:errlen-1)

            end if
         end block
         errflag = .true.
         return
      end if

      ! Cから返されたポインタをFortranのポインタに関連付ける。
      call c_f_pointer(cptr_siz, fptr, shape=[length])

      ! 関数c_PQ_conninfo_parseを呼び出して、結果をcptr_objに格納する。ここでのエラーメッセージは捨てる。
      block 
         type(c_ptr) :: trush 
         cptr_obj = c_PQ_conninfo_parse(c_conninfo, trush)
      end block
      
      ! Cから返されたポインタをFortranポインタに関連付ける。
      call c_f_pointer(cptr_obj, opts_ptr, shape=[length])
      
      ! オプション情報を配列optionsにコピーする。
      allocate(options(length))
      do i = 1, length
         call read_option(fptr(i), opts_ptr(i), options(i))
      end do

      ! 使用済みメモリを解放する。
      call c_PQ_conninfo_parse_free(cptr_siz)
      call PQconninfoFree(cptr_obj)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNINFOPARSE)
   end subroutine PQconninfoParse


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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-misc.html#LIBPQ-PQCONNINFOFREE)
   end subroutine PQconninfoFree


   subroutine PQfinish(conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(inout) :: conn

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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQFINISH)
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQRESET)
   end subroutine PQreset
            

   function PQresetStart(conn)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: PQresetStart

      interface
         function c_PQ_reset_start (conn) bind(c, name="PQresetStart")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_reset_start
         end function c_PQ_reset_start
      end interface 

      PQresetStart = c_PQ_reset_start(conn)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQRESETSTART)
   end function PQresetStart

   
   function PQresetPoll (conn)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: PQresetPoll

      interface
         function c_PQ_reset_poll (conn) bind(c, name="PQresetPoll")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_reset_poll
         end function c_PQ_reset_poll
      end interface

      PQresetPoll = c_PQ_reset_poll(conn)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQRESETSTART)
   end function PQresetPoll


   function PQpingParams (keywords, values, expand_dbname) result(res)
      use :: character_operations_m
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
            type(c_ptr), intent(in), value :: keywords
            type(c_ptr), intent(in), value :: values
            integer(c_int), intent(in) :: expand_dbname
            integer(c_int) :: c_PQ_ping_params
         end function c_PQ_ping_params
      end interface

      max_len_key = max_length_char_array(keywords)
      max_len_val = max_length_char_array(values)

      block
         character(max_len_key+1, kind=c_char), allocatable, target :: c_keys(:)
         character(max_len_val+1, kind=c_char), allocatable, target :: c_values(:)

         type(c_ptr), allocatable, target :: ptr_keys(:)
         type(c_ptr), allocatable, target :: ptr_values(:)

         integer(c_int) :: c_expand_dbname

         call cchar_array_from_strings(keywords, c_keys, max_len_key)
         call cptr_array_from_cchar(c_keys, ptr_keys)

         call cchar_array_from_strings(values, c_values, max_len_val)
         call cptr_array_from_cchar(c_values, ptr_values)

         c_expand_dbname = expand_dbname

         res = c_PQ_ping_params(c_loc(ptr_keys(1)), c_loc(ptr_values(1)), c_expand_dbname)
      end block

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQPINGPARAMS)
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQPING)
   end function PQping


   ! function PQsetSSLKeyPassHook_OpenSSL
   ! function PQgetSSLKeyPassHook_OpenSSL
   

   !==================================================================!
   ! Connection Status Functions


   function PQdb (conn) result(res)
      use :: character_operations_m
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQDB)
   end function PQdb


   function PQuser (conn) result(res)
      use :: character_operations_m
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQUSER)
   end function PQuser


   function PQpass (conn) result(res)
      use :: character_operations_m
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      interface
         function c_PQ_pass(conn) bind(c, name="PQpass")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_pass
         end function c_PQ_pass
      end interface

      res => c_to_f_charpointer(c_PQ_pass(conn))
   
      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQPASS)
   end function PQpass



   function PQhost (conn) result(res)
      use :: character_operations_m
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQHOST)
   end function PQhost


   function PQhostaddr (conn) result(res)
      use :: character_operations_m
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQHOSTADDR)
   end function PQhostaddr

   
   function PQport(conn) result(res)
      use :: character_operations_m
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      interface
         function c_PQ_port(conn) bind(c, name="PQport")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_port
         end function c_PQ_port
      end interface

      res => c_to_f_charpointer(c_PQ_port(conn))

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQPORT)
   end function PQport


   function PQoptions (conn) result(res)
      use :: character_operations_m
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
   
      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQOPTIONS)
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSTATUS)
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

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQTRANSACTIONSTATUS)
   end function PQtransactionStatus


   function PQparameterStatus(conn, paramName)
      use :: character_operations_m
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: paramName

      character(:), allocatable :: c_paramName
      character(:), pointer :: PQparameterStatus

      interface
         function c_PQ_parameter_status(conn, param_name) bind(c, name="PQparameterStatus")
            import c_ptr, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: param_name(*)
            type(c_ptr) :: c_PQ_parameter_status
         end function c_PQ_parameter_status
      end interface

      c_paramName = trim(adjustl(paramName))//c_null_char

      PQparameterStatus => c_to_f_charpointer(c_PQ_parameter_status(conn, c_paramName))

      if (.not. associated(PQparameterStatus)) then
         PQparameterStatus = ''
      end if

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQPARAMETERSTATUS)
   end function PQparameterStatus
   

   function PQprotocolVersion(conn) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      integer :: res

      interface
         function c_PQ_protocol_version (conn) bind(c, name="PQprotocolVersion")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_protocol_version
         end function c_PQ_protocol_version
      end interface

      res = c_PQ_protocol_version(conn)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQPROTOCOLVERSION)
   end function PQprotocolVersion


   function PQserverVersion (conn) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      integer :: res

      interface
         function c_PQ_server_version (conn) bind(c, name="PQserverVersion")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_server_version
         end function c_PQ_server_version
      end interface

      res = c_PQ_server_version(conn)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSERVERVERSION)
   end function PQserverVersion
   

   !>> Returns the error message most recently generated by an operation on the connection.
   !>
   function PQerrorMessage(conn)
      use ::  character_operations_m
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

      !*> Nearly all libpq functions will set a message for `PQerrorMessage` if they fail. 
      ! > Note that by libpq convention, a nonempty `PQerrorMessage` result can consist of multiple lines,
      ! > and will include a trailing newline. The caller should not free the result directly. 
      ! > It will be freed when the associated PGconn handle is passed to `[[PQfinish]]`. 
      ! > The result string should not be expected to remain the same across operations on the `PGconn` structure.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQERRORMESSAGE)
   end function PQerrorMessage


   !>> Obtain the file descriptor number of the connection socket to the server. 
   !>> 
   function PQsocket (conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(c_int) :: PQsocket

      interface
         function c_PQ_socket(conn) bind(c, name="PQsocket")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_socket
         end function c_PQ_socket
      end interface

      PQsocket = c_PQ_socket(conn)

      !*> A valid descriptor will be greater than or equal to 0; a result of -1 indicates that no server connection is
      ! > currently open. (This will not change during normal operation, but could change during connection set up or 
      ! > reset.)
      ! >
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSOCKET)
   end function PQsocket


   !>> Returns the process ID (PID) of the backend process handling this connection.
   !>> 
   function PQbackendPID(conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(c_int) :: PQbackendPID

      interface
         function c_PQ_backend_pid(conn) bind(c, name="PQbackendPID")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_backend_pid
         end function c_PQ_backend_pid
      end interface

      PQbackendPID = c_PQ_backend_pid(conn)

      !*> The backend PID is useful for debugging purposes and for comparison to `NOTIFY` messages (which include the PID
      ! > of the notifying backend process). Note that the PID belongs to a process executing on the database server host,
      ! > not the local host!
      ! >
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQBACKENDPID)
   end function PQbackendPID


   function PQconnectionNeedsPassword(conn)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      type(c_ptr), intent(in) :: conn
      integer(int32) :: PQconnectionNeedsPassword

      interface
         function c_PQ_connection_needs_password (conn)  &
                  bind(c, name="PQconnectionNeedsPassword") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: res
         end function c_PQ_connection_needs_password
      end interface

      PQconnectionNeedsPassword = c_PQ_connection_needs_password(conn)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQCONNECTIONNEEDSPASSWORD)
   end function PQconnectionNeedsPassword


   function PQconnectionUsedPassword (conn)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      type(c_ptr), intent(in) :: conn
      integer(int32) :: PQconnectionUsedPassword

      interface
         function c_PQ_connection_used_password (conn) &
                  bind(c, name="PQconnectionUsedPassword") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: res
         end function c_PQ_connection_used_password
      end interface

      PQconnectionUsedPassword = c_PQ_connection_used_password(conn)

      !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQCONNECTIONUSEDPASSWORD)
   end function PQconnectionUsedPassword


   != for SSL connection
   function PQsslInUse (conn)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      logical :: PQsslInUse

      integer(int32) :: res

      interface
         function c_PQ_ssl_in_use (conn) bind(c, name="PQsslInUse")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_ssl_in_use 
         end function c_PQ_ssl_in_use
      end interface

      res = c_PQ_ssl_in_use(conn)

      if (res == 1) then
         PQsslInUse = .true.
      else if (res == 0) then
         PQsslInUse = .false.
      end if

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSSLINUSE)
   end function PQsslInUse
      
   subroutine PQsslAttribute(conn, attribute_name, resultstr)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: character_operations_m
      implicit none

      ! Input parameters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: attribute_name

      ! Output pointer
      character(:), allocatable :: resultstr

      ! Local variables
      character(:, kind=c_char), allocatable :: c_attribute_name
      character(:), allocatable :: buff 
      type(c_ptr) :: cptr

      interface
         function c_PQ_ssl_attribute(conn, attribute_name) bind(c, name="PQsslAttribute")
            import c_ptr, c_char
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: attribute_name
            type(c_ptr) :: c_PQ_ssl_attribute
         end function c_PQ_ssl_attribute
      end interface

      c_attribute_name = trim(adjustl(attribute_name))//c_null_char

      cptr = c_PQ_ssl_attribute(conn, c_attribute_name)

      if (c_associated(cptr)) then
         call c_char_to_f_string(cptr, buff)

         resultstr = buff
      else
         resultstr = ''
      end if 

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSSLATTRIBUTE)
   end subroutine PQsslAttribute


   !> Subroutine for retrieving SSL attribute names from a PostgreSQL 
   !> database connection and populating an array.
   ! Input:
   !  - 'conn' is the connection pointer,
   !  - 'len' is the length of the element of the output string array.
   ! Output:
   !  - 'strings' must be a one-dimensional array of uninitialized
   !    strings that will be allocated in this subroutine.
   !
   subroutine PQsslAttributeNames (conn, strings, len)
      !!@bug This doesn't work in the GCC 11.4.0 environment. @endbug 
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: character_operations_m
      implicit none

      ! Input Parameters
      type(c_ptr), intent(in) :: conn
      integer(int32), intent(in) :: len
      character(len), allocatable :: strings(:)

      ! Interfaces to functions in src/wraps/PQsslAttributeNamesWrap.c
      interface
         function c_PQ_ssl_Attribute_Names_Prepare_Size(conn) &
               bind(c, name="PQsslAttributeNamesPrepareSize")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_ssl_Attribute_Names_Prepare_Size
         end function c_PQ_ssl_Attribute_Names_Prepare_Size
      end interface 

      interface
         subroutine c_PQ_ssl_Attribute_Names_Prepare_Lengths (conn, array) &
               bind(c, name="PQsslAttributeNamesPrepareLengths")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: array(:)
         end subroutine c_PQ_ssl_Attribute_Names_Prepare_Lengths
      end interface
      
      ! Interface to libpq function PQsslAttriubteNames
      interface
         function c_PQ_ssl_Attribute_Names_Wraped (conn) bind(c, name="PQsslAttributeNames")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_ssl_Attribute_Names_Wraped
         end function c_PQ_ssl_Attribute_Names_Wraped
      end interface

      ! Local variables
      integer(int32) :: siz                     ! Size of the attribute names array
      integer(int32), allocatable :: lengths(:) ! Array to store lengths
      type(c_ptr) :: ptr                        ! Pointer to the attribute names array

      ! Get the size of the attribute names array using C funciton.
      siz = c_PQ_ssl_Attribute_Names_Prepare_Size(conn)

      ! Allocate memory for the lengths array and the strings array.
      allocate(lengths(siz))
      allocate(strings(siz))

      ! Get the lengths of attribute name strings using C funciton.
      call c_PQ_ssl_Attribute_Names_Prepare_Lengths(conn, lengths)

      ! Get a pointer to the attribute names array using PostgreSQL function.
      ptr = c_PQ_ssl_Attribute_Names_Wraped(conn)

      ! If the pointer is not associated, exit.
      if (.not. c_associated(ptr)) then
         deallocate(lengths)
         return
      end if

      ! Process strings within a block
      block
         character(:), pointer :: strptr                 ! Temporary string pointer
         type(c_ptr), dimension(:), pointer :: ptr_array ! Array of pointers
         integer(int32) :: i

         ! Associate the poninter with a Fortran array
         call c_f_pointer(ptr, ptr_array, shape=[siz])

         do i = 1, siz

            ! Convert the pointer to a Fortran character pointer with its length.
            strptr => c_to_f_charpointer_with_length(ptr_array(i), lengths(i))

            ! Store the string in the strings array.
            strings(i) = strptr(:)
         end do

      end block 

      deallocate(lengths)
      
   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSSLATTRIBUTENAMES)
   end subroutine PQsslAttributeNames


   ! function PQsslStruct


!==================================================================!
!== Canceling Queries in Progress

   function PQgetCancel (conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      type(c_ptr) :: PQgetCancel

      interface
         function c_PQ_get_cancel(conn) bind(c, name="PQgetCancel")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_get_cancel
         end function c_PQ_get_cancel
      end interface

      PQgetCancel = c_PQ_get_cancel(conn)

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-cancel.html#LIBPQ-PQGETCANCEL)
   end function PQgetCancel

   
   subroutine PQfreeCancel (cancel)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: cancel
      
      interface
         subroutine c_PQ_free_cancel(cancel) bind(c, name="PQfreeCancel")
            import c_ptr 
            implicit none
            type(c_ptr), intent(in), value :: cancel
         end subroutine
      end interface

      call c_PQ_free_cancel(cancel)

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-cancel.html#LIBPQ-PQFREECANCEL)
   end subroutine PQfreeCancel

   !>> Requests that the server abandon processing of the current command.
   !>> 
   function PQcancel (cancel, errbuf, errbufsize)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      ! Input parameters
      type(c_ptr), intent(in) :: cancel
      character(*), intent(inout) :: errbuf
      integer(int32), intent(in) :: errbufsize

      ! Output integer
      integer(int32) :: PQcancel

      ! Local variables
      character(len=256, kind=c_char), target :: c_errbuf 

      interface
         function c_PQ_cancel (cancel, errbuf, errbufsize) bind(c, name="PQcancel")
            import c_int, c_ptr, c_char
            implicit none
            type(c_ptr), intent(in), value :: cancel
            type(c_ptr), intent(in), value :: errbuf
            integer(c_int), intent(in), value :: errbufsize
            integer(c_int) :: c_PQ_cancel
         end function c_PQ_cancel
      end interface

      c_errbuf = ''

      PQcancel = c_PQ_cancel(cancel, c_loc(c_errbuf), 256)


      errbuf = c_errbuf(1:errbufsize)

      !*> The return value is `1` if the cancel request was successfully dispatched and `0` if not.
      ! > If not, `errbuf` is filled with an explanatory error message. `errbuf` must be a char array
      ! > of size `errbufsize` (the recommended size is 256 bites).
      ! >
      
      !*> Successful dispatch is no guarantee that the request will have any effect, however.
      ! > If the cancellation is effective, the current command will terminate early and return an error
      ! > result. If the cancellation fails (say, because the server was already done processing the command),
      ! > then there will be no visible result at all.
      ! > 
      
      !*> `PQcancel` can safely be invoked from a signal handler, if the `errbuf` is a local variable in the
      ! > signal handler. The `PGcancel` object is read-only as far as `PQcancel` is concerned, so it 
      ! > can also be invoked from a thread that is separate from the one manipulating the `PGconn` object.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-cancel.html#LIBPQ-PQCANCEL)
   end function PQcancel


!==================================================================!
!== Control Functions

   function PQclientEncoding(conn) result(res)
      use character_operations_m
      use fe_exec_m, only: PQfreemem
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      !* Returns the client encoding.
      !  
      !  It returns the encoding string such as `UTF8`.
      !  This function wraps both `PQclientEncoding` and `pg_encoding_to_char`.

      ! Input parameter
      type(c_ptr), intent(in) :: conn
      
      ! Declare Result pointer
      character(:), pointer :: res

      ! Declare local variables
      character(:), allocatable, target, save :: encoding
      integer(c_int) :: encoding_id
      type(c_ptr) :: cptr

      ! Define an interface for the C func. that retrieves the clinent encoding id.
      interface
         function c_PQ_client_encoding (conn) bind(c, name="PQclientEncoding")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_client_encoding
         end function c_PQ_client_encoding
      end interface

      ! Define an interface for the C func. that converts encoding-id to a character string.
      interface
         function c_pg_encoding_to_char(encoding_id) bind(c, name="pg_encoding_to_char")
            import c_ptr, c_int
            integer(c_int), intent(in), value :: encoding_id
            type(c_ptr) :: c_pg_encoding_to_char
         end function c_pg_encoding_to_char
      end interface

      ! Initialize the result pointer to null.
      nullify(res)

      ! Call the C function to retrieve the client encoding id.
      encoding_id = c_PQ_client_encoding(conn)
      
      ! If the returned value is -1, return without setting the result (indicating an error).
      if (encoding_id == -1) return

      ! Call the C function to convert the encoding id to a character string (c_ptr)
      cptr = c_pg_encoding_to_char(encoding_id)

      ! Convert the C string to a Fortran string and assign it to the result.
      call c_char_to_f_string(cptr, encoding)
      res => encoding

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-control.html#LIBPQ-PQCLIENTENCODING)
   end function 


   function PQsetClientEncoding (conn, encoding) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: encoding

      integer(int32) :: res

      character(:, kind=c_char), allocatable :: c_encoding

      interface
         function c_PQ_set_client_encoding(conn, encoding) bind(c, name="PQsetClientEncoding")
            import c_ptr, c_char, c_int
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: encoding(*)
            integer(c_int) :: c_PQ_set_client_encoding
         end function c_PQ_set_client_encoding
      end interface

      c_encoding = trim(encoding)//c_null_char

      res = c_PQ_set_client_encoding(conn, c_encoding)


   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-control.html#LIBPQ-PQSETCLIENTENCODING)
   end function PQsetClientEncoding


   function PQsetErrorVerbosity (conn, verbosity) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: enumerators_t
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32), intent(in) :: verbosity

      integer(int32) :: res

      interface
         function c_PQ_set_Error_Verbosity(conn, verbosity) bind(c, name="PQsetErrorVerbosity")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int), intent(in), value :: verbosity
            integer(c_int) :: c_PQ_set_error_verbosity
         end function c_PQ_set_error_verbosity
      end interface 

      select case (verbosity)
      case (PQERRORS_TERSE)
         res = c_PQ_set_Error_Verbosity(conn, PQERRORS_TERSE)
      case (PQERRORS_DEFAULT)
         res = c_PQ_set_Error_Verbosity(conn, PQERRORS_DEFAULT)
      case (PQERRORS_VERBOSE)
         res = c_PQ_set_Error_Verbosity(conn, PQERRORS_VERBOSE)
      case (PQERRORS_SQLSTATE)
         res = c_PQ_set_Error_Verbosity(conn, PQERRORS_SQLSTATE)
      case default 
         res = -1
      end select

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-control.html#LIBPQ-PQSETERRORVERBOSITY)
   end function PQsetErrorVerbosity



   function PQsetErrorContextVisibility (conn, show_context) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: enumerators_t
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32), intent(in) :: show_context

      integer (int32) :: res

      interface
         function c_PQ_set_error_context_visibility(conn, show_context) &
               bind(c, name="PQsetErrorContextVisibility")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int), intent(in), value :: show_context
            integer(c_int) :: c_PQ_set_error_context_visibility
         end function
      end interface

      select case (show_context)
      case (PQSHOW_CONTEXT_NEVER)
         res = c_PQ_set_error_context_visibility(conn, PQSHOW_CONTEXT_NEVER)
      case (PQSHOW_CONTEXT_ERRORS)
         res = c_PQ_set_error_context_visibility(conn, PQSHOW_CONTEXT_ERRORS)
      case (PQSHOW_CONTEXT_ALWAYS)
         res = c_PQ_set_error_context_visibility(conn, PQSHOW_CONTEXT_ALWAYS)
      case default
         res = -1
      end select

   !* cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-control.html#LIBPQ-PQSETERRORCONTEXTVISIBILITY)
   end function PQsetErrorContextVisibility
   

end module fe_connect_m