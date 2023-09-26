module m_fe_connect
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


   ! PRIVATE functions
   private :: PQconnectdbParams_back


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
      use, intrinsic :: iso_c_binding
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)
      integer :: expand_dbname
      type(c_ptr) :: conn

      conn =  PQconnectdbParams_back(keywords, values, expand_dbname, .false.)

   end function PQconnectdbParams
      

   function PQconnectdbParams_back (keywords, values, expand_dbname, isNonblocking) result(conn)
      use :: character_operations
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


   function PQconnectStartParams(keywords, values, expand_dbname) result(conn)
      use, intrinsic :: iso_c_binding
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)
      integer :: expand_dbname
      type(c_ptr) :: conn

      conn =  PQconnectdbParams_back(keywords, values, expand_dbname, .true.)

   end function PQconnectStartParams


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

   end function PQconnectStart


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

   end function PQconnectPoll


   subroutine PQconndefaults (options)
      use :: t_PQconninfoOption
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

      contains

         subroutine read_option(sizes, c_option, option)
            use :: t_PQconninfoOption
            use, intrinsic :: iso_c_binding
            implicit none
            type(c_PQconnoptionSizes), intent(in) :: sizes
            type(c_PQconninfoOption), intent(inout) :: c_option
            type(PQconninfoOption), intent(out) :: option

            ! Cの構造体からFortranの派生型に、keywordの値をコピーする。
            block
               character(sizes%keyword), pointer :: keyword
               call c_f_pointer(c_option%keyword, keyword)
               option%keyword = trim(keyword)
            end block

            if (sizes%envvar > 0) then
               block
                  character(sizes%envvar), pointer :: envvar
                  call c_f_pointer(c_option%envvar, envvar)
                  option%envvar = trim(envvar)
               end block
            else 
               option%envvar = '' 
            end if

            if (sizes%compiled >0) then
               block
                  character(sizes%compiled), pointer :: compiled
                  call c_f_pointer(c_option%compiled, compiled)
                  option%compiled = trim(compiled)
               end block 
            else
               option%compiled = ''
            end if

            if (sizes%val >0) then
               block
                  character(sizes%val), pointer :: val
                  call c_f_pointer(c_option%val, val)
                  option%val = trim(val)
               end block
            else
               option%val = ''
            end if

            if (sizes%label > 0) then
               block
                  character(sizes%label), pointer :: label
                  call c_f_pointer(c_option%label, label)
                  option%label = trim(label)
               end block
            else
               option%label = ''
            end if

            if (sizes%dispchar > 0) then
               block
                  character(1), pointer :: dispchar
                  call c_f_pointer(c_option%dispchar, dispchar)
                  option%dispchar = trim(dispchar)
               end block 
            else
               option%dispchar = ''
            end if

            block
               option%dispsize = c_option%dispsize
            end block

         end subroutine read_option

   end subroutine PQconndefaults


   subroutine PQconninfo (conn, options)
      use :: character_operations
      use :: t_PQconninfoOption
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


   end subroutine PQconninfo
            

   subroutine PQconninfoParse(conninfo, options, errmsg, errflag)
      use :: t_PQconninfoOption
      use :: character_operations
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

   end function PQresetPoll


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


   function PQpass (conn) result(res)
      use :: character_pointer_wrapper
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
   
   end function PQpass



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

   
   function PQport(conn) result(res)
      use :: character_pointer_wrapper
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

   end function PQport


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


   function PQparameterStatus(conn, paramName)
      use :: character_pointer_wrapper
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
   
   end function PQserverVersion
   


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

   end function PQsocket


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

   end function PQsslInUse
      
   subroutine PQsslAttribute(conn, attribute_name, resultstr)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: character_pointer_wrapper
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

   end subroutine PQsslAttribute


   ! Subroutine for retrieving SSL attribute names from a PostgreSQL 
   ! database connection and populating an array.
   ! Input:
   !  - 'conn' is the connection pointer,
   !  - 'len' is the length of the element of the output string array.
   ! Output:
   !  - 'strings' must be a one-dimensional array of uninitialized
   !    strings that will be allocated in this subroutine.
   subroutine PQsslAttributeNames (conn, strings, len)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: character_pointer_wrapper
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
   end subroutine PQfreeCancel


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

   end function PQcancel


!==================================================================!
!== Control Functions

   function PQclientEncoding(conn) result(res)
      use character_pointer_wrapper
      use m_fe_exec, only: PQfreemem
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

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


   end function PQsetClientEncoding


   function PQsetErrorVerbosity (conn, verbosity) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

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

      res = c_PQ_set_Error_Verbosity(conn, verbosity)

   end function PQsetErrorVerbosity

   ! function PQsetErrorContextVisibility
   

end module m_fe_connect