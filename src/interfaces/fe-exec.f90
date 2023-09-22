module m_fe_exec
   implicit none
   private

   public :: PQexec
   public :: PQresultStatus
   public :: PQresultErrorMessage
   public :: PQresultVerboseErrorMessage
   public :: PQgetvalue
   public :: PQntuples
   public :: PQnfields
   public :: PQfname
   public :: PQfnumber
   public :: PQclear
   public :: PQgetisnull
   public :: PQfreemem
   public :: PQbinaryTuples
   public :: PQfformat
   public :: PQfmod
   public :: PQfsize
   public :: PQftablecol
   public :: PQftable
   public :: PQftype
   public :: PQresStatus
   public :: PQgetlength
   public :: PQnparams
   public :: PQparamtype
   public :: PQresultErrorField
   public :: PQcmdStatus
   public :: PQcmdTuples
   public :: PQoidValue

   public :: PQsendQuery
   public :: PQgetResult
   public :: PQconsumeInput
   public :: PQisBusy
   public :: PQsetnonblocking
   public :: PQisnonblocking
   public :: PQflush
   public :: PQescapeLiteral

contains


   !==================================================================!
   ! Command Execution Functions

   !== Main Functions

   function PQexec(conn, query) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: query
      character(:, kind=c_char), allocatable :: c_query
      type(c_ptr) :: res
      
      interface
         ! Interface to PQexec in interfaces/libpq/fe-exec.c:
         !
         ! PGresult *PQexec(PGconn *conn, const char *query)
         !
         function c_PQ_exec (conn, query) bind(c, name='PQexec') result(pgresult)
            import c_ptr, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            
            ! To pass a string to C, declare an array of type 'character' with kind 'c_char'.
            character(1, kind=c_char), intent(in) :: query(*)

            type(c_ptr) :: pgresult
         end function c_PQ_exec
      end interface

      ! Append a C null character to the end of the query.
      c_query = query//c_null_char

      res = c_PQ_exec(conn, c_query)

   end function PQexec

   
   ! function PQexecParams
   ! function PQprepare
   ! function PQexecPrepared
   ! function PQdescribePrepared
   ! function PQdescribePortal


   function PQresultStatus(pgresult) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: pgresult
      integer(int32) :: res

      interface
         ! Interface to PQresultStatus in interfaces/libpq/fe-exec.c:
         !
         ! ExecStatusType PQresultStatus(const PGresult *res)
         !
         function c_PQ_result_status(pgresult) bind(c, name='PQresultStatus') result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int) :: res
         end function c_PQ_result_status
      end interface
      
      res = c_PQ_result_status(pgresult)

   end function PQresultStatus


   function PQresStatus(status) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      ! 入力
      integer(int32), intent(in) :: status
      
      ! 出力：文字列へのポインタ
      character(:), pointer :: res

      ! C関数の結果を代入するCポインタ型の変数
      type(c_ptr) :: status_description

      ! C関数へのインターフェース
      interface
         function c_PQ_res_status (status) bind(c, name="PQresStatus") result(res)
            import c_ptr, c_int
            integer(c_int), intent(in), value :: status
            type(c_ptr) :: res
         end function
      end interface

      ! C関数を呼び出して、結果のポインタをstatus_descriptionに受け取る。
      status_description = c_PQ_res_status(status)

      ! 結果のCポインタをFortranの文字列ポインタに変換する。
      res => c_to_f_charpointer(status_description)

   end function PQresStatus


   function PQresultErrorMessage(pgresult) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: pgresult
      character(:, kind=c_char), pointer :: res

      interface
         ! Interface to PQresultErrorMessage in interface/libpq/fe-exec.c:
         !
         ! char *PQresultErrorMessage(const PGresult *res)
         !
         function c_PQ_result_error_message (res) bind(c, name='PQresultErrorMessage')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: res
            type(c_ptr) :: c_PQ_result_error_message
         end function c_PQ_result_error_message
      end interface

      res => c_to_f_charpointer(c_PQ_result_error_message(pgresult))

   end function PQresultErrorMessage


   function PQresultVerboseErrorMessage(pgresult, verbosity, show_context) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: verbosity
      integer(int32), intent(in) :: show_context

      character(:, kind=c_char), pointer :: res

      interface
         function c_PQ_result_verbose_error_message (res, verbosity, show_context ) &
                                          bind(c, name='PQresultVerboseErrorMessage')
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: res
            integer(c_int), intent(in), value :: verbosity, show_context
            type(c_ptr) :: c_PQ_result_verbose_error_message
         end function c_PQ_result_verbose_error_message
      end interface

      res => c_to_f_charpointer(c_PQ_result_verbose_error_message(pgresult, verbosity, show_context))
   end function PQresultVerboseErrorMessage


   function PQresultErrorField(pgresult, fieldcode) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: fieldcode
      character(:, kind=c_char), pointer :: res

      interface
         ! Interface to PQresultErrorField in interface/libpq/fe-exec.c:
         !
         ! char *PQresultErrorField(const PGresult *res)
         !
         function c_PQ_result_error_field (res, fieldcode) bind(c, name='PQresultErrorField')
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: res
            integer(c_int), intent(in), value :: fieldcode
            type(c_ptr) :: c_PQ_result_error_field
         end function c_PQ_result_error_field
      end interface

      res => c_to_f_charpointer(c_PQ_result_error_field(pgresult, fieldcode))
   end function PQresultErrorField


   !-- Delete a PGresult
   subroutine PQclear(res)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: res

      interface
         ! Interface to PQclear in interface/libpq/fe-exec.c:
         !
         ! void PQclear(PGresult *res)
         !
         subroutine c_PQ_clear(res) bind(c, name='PQclear')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: res
         end subroutine c_PQ_clear
      end interface

      call c_PQ_clear(res)

   end subroutine PQclear

   
   !== Retrieving Query Result Information

   function PQntuples(pgresult) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: pgresult
      integer(int32) :: res

      interface
         ! Interface to PQntuples in interface/libpq/fe-exec.c:
         !
         ! int PQntuples(const PGresult *res)
         !
         function c_PQ_n_tuples (pgresult) bind(c, name='PQntuples')
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int) :: c_PQ_n_tuples
         end function c_PQ_n_tuples
      end interface

      res = c_PQ_n_tuples(pgresult)

   end function PQntuples


   function PQnfields(pgresult) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: pgresult
      integer(int32) :: res

      interface
         ! Interface to PQnfields in interface/libpq/fe-exec.c:
         !
         ! int PQnfields(const PGresult *res)
         !
         function c_PQ_n_fields (pgresult) bind(c, name='PQnfields')
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int) :: c_PQ_n_fields
         end function c_PQ_n_fields
      end interface

      res = c_PQ_n_fields(pgresult)
   end function PQnfields

   function PQfname(pgresult, field_num) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_fortran_env, only:int32
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
      implicit none
      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: field_num
      character(:, kind=c_char), pointer :: res

      interface
         ! Interface to PQfname in src/interface/libpq/fe-exec.c:
         !
         ! char *PQfname(const PGresult *res, int field_num)
         function c_PQ_field_name (pgresult, c_field_num) bind(c, name='PQfname')
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: c_field_num
            type(c_ptr) :: c_PQ_field_name
         end function c_PQ_field_name
      end interface
      
      res => c_to_f_charpointer(c_PQ_field_name(pgresult, field_num ))

   end function PQfname

   
   function PQfnumber (pgresult, column_name)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: pgresult
      character(*), intent(in) :: column_name
      character(:, kind=c_char), allocatable :: c_column_name
      integer :: PQfnumber

      interface 
         function c_PQ_field_number(pgresult, c_name) bind(c, name='PQfnumber') &
                                                      result(res)
            import c_ptr, c_int, c_char
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            character(1, kind=c_char), intent(in) :: c_name(*)
            integer(c_int) :: res
         end function c_PQ_field_number
      end interface

      c_column_name = trim(adjustl(column_name))//c_null_char

      PQfnumber = c_PQ_field_number(pgresult, c_column_name)

   end function PQfnumber

   
   function PQftable (pgresult, column_number) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: unsigned

      ! 入力
      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: column_number

      ! 出力結果
      integer(int64) :: res

      ! カラムの属するテーブルのOidを格納する変数を宣言する。
      type(uint32) :: oid

      ! C関数 PQftableへのインターフェースを定義する。
      interface
         function c_PQ_field_table(pgresult, column_number) bind(c, name="PQftable")
            import uint32, c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult          ! PostgreSQLの結果オブジェクト
            integer(c_int), intent(in), value :: column_number  ! カラム番号
            type(uint32) :: c_PQ_field_table                    ! 戻り値は符号なし整数型
         end function 
      end interface

      ! C関数を呼び出して、Oidを取得する。
      oid = c_PQ_field_table(pgresult, column_number)

      ! カラムのOidを整数型に変換して結果に格納する。
      res = int(oid)

   end function PQftable


   function PQftablecol (pgresult, column_number) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: column_number
      integer(int32) :: res

      interface
         function c_PQ_field_table_column(pgresult, column_number) &
                                 bind(c, name="PQftablecol") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: column_number
            integer(c_int) :: res
         end function c_PQ_field_table_column
      end interface

      res = c_PQ_field_table_column(pgresult, column_number)

   end function PQftablecol


   function PQfformat (pgresult, column_number) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: column_number
      integer(int32) :: res

      interface
         function c_PQ_field_format (pgresult, column_number) &
                                 bind(c, name="PQfformat") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: column_number
            integer(c_int) :: res
         end function c_PQ_field_format
      end interface

      res = c_PQ_field_format(pgresult, column_number)

   end function PQfformat


   function PQftype(pgresult, column_number) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: unsigned

      ! 入力
      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: column_number

      ! 出力結果
      integer(int64) :: res

      ! カラムに関連したデータ型を返す。返される整数はその型の内部的なOID番号である。
      type(uint32) :: oid

      interface
         function c_PQ_field_type (pgresult, column_number) bind(c, name="PQftype")
            import uint32, c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: column_number
            type(uint32) :: c_PQ_field_type
         end function c_PQ_field_type
      end interface

      ! C関数を呼び出して、Oidを取得する。
      oid = c_PQ_field_type(pgresult, column_number)

      ! カラムのOidを64ビット整数型に変換して結果を格納する。
      res = int(oid)

   end function PQftype
   

   function PQfmod(pgresult, column_number) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: column_number
      integer(int32) :: res

      interface
         function c_PQ_field_modifier (pgresult, column_number) &
                                 bind(c, name="PQfmod") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: column_number
            integer(c_int) :: res
         end function c_PQ_field_modifier
      end interface

      res = c_PQ_field_modifier(pgresult, column_number)

   end function PQfmod


   function PQfsize (pgresult, column_number) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: column_number
      integer(int32) :: res

      interface
         function c_PQ_field_size (pgresult, column_number) &
                                 bind(c, name="PQfsize") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: column_number
            integer(c_int) :: res
         end function c_PQ_field_size
      end interface

      res = c_PQ_field_size(pgresult, column_number)

   end function PQfsize


   function PQbinaryTuples (pgresult)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(int32) :: res
      logical :: PQbinaryTuples

      interface
         function c_PQ_binary_tuples (pgresult) &
                                 bind(c, name="PQbinaryTuples") result(res)
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int) :: res
         end function c_PQ_binary_tuples
      end interface
         
      res = c_PQ_binary_tuples(pgresult)
      
      if (res == 1) then
         PQbinaryTuples = .true.

      else
         PQbinaryTuples = .false.

      end if

   end function PQbinaryTuples

  

   function PQgetvalue (pgresult, tuple_num, field_num)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(c_int), intent(in) :: tuple_num, field_num
      character(:, c_char), pointer :: PQgetvalue

      interface
         ! Interface to PQgetvalue in interface/libpq/fe-exec.c:
         !
         ! char *PQgetvalue(const PGresult *res, int tup_num, int field_num)
         !
         function c_PQ_get_value (res, tup_num, field_num) &
                                           bind(c, name='PQgetvalue')
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: res
            integer(c_int), intent(in), value :: tup_num, field_num
            type(c_ptr):: c_PQ_get_value
         end function c_PQ_get_value
      end interface
      
      ! 
      PQgetvalue => &
         c_to_f_charpointer( &
            c_PQ_get_value( pgresult, tuple_num, field_num) &
         )

   end function PQgetvalue


   function PQgetisnull (pgresult, row_number, column_number)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      
      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: row_number, column_number
      logical :: PQgetisnull

      interface
         function c_PQ_get_is_null (res, row_number, column_number) bind(c, name="PQgetisnull")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: res
            integer(c_int), intent(in), value :: row_number
            integer(c_int), intent(in), value :: column_number
            integer(c_int) :: c_PQ_get_is_null
         end function c_PQ_get_is_null
      end interface

      block
         integer :: func_result

         func_result = c_PQ_get_is_null(pgresult, row_number, column_number)

         PQgetisnull = .false.

         if (func_result == 0) then 
            PQgetisnull = .false.
         
         else if (func_result == 1 ) then
            PQgetisnull = .true.
         end if
         
      end block

   end function PQgetisnull


   function PQgetlength(pgresult)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding

      type(c_ptr), intent(in) :: pgresult

      integer(int32) :: PQgetlength

      interface
         function c_PQ_get_length (pgresult) bind(c, name="PQgetlength")
            import c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int) :: c_PQ_get_length
         end function c_PQ_get_length
      end interface

      PQgetlength = c_PQ_get_length(pgresult)

   end function PQgetlength

      
   function PQnparams(pgresult)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in) :: pgresult
      integer(int32) :: PQnparams

      interface
         function c_PQ_nparams (pgresult) bind(c, name="PQnparams")
            import c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int) :: c_PQ_nparams
         end function 
      end interface

      PQnparams = c_PQ_nparams(pgresult)

   end function PQnparams

   
   function PQparamtype(pgresult, param_number) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned

      type(c_ptr), intent(in) :: pgresult
      integer(int32), intent(in) :: param_number

      integer(int64) :: res

      type(uint32) :: oid

      interface 
         function c_PQ_parameter_type(pgresult, param_number) bind(c, name="PQparamtype") result(res)
            import uint32, c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            integer(c_int), intent(in), value :: param_number
            type(uint32) :: res
         end function c_PQ_parameter_type
      end interface

      oid = c_PQ_parameter_type(pgresult, param_number)

      res = int(oid)

   end function PQparamtype
      
   

   !== Retrieving Other Result Information

   function PQcmdStatus(pgresult) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: pgresult
      character(:), allocatable, target, save :: str
      character(:), pointer :: res

      interface
         function c_PQ_command_status(pgresult) bind(c, name="PQcmdStatus")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            type(c_ptr) :: c_PQ_command_status
         end function c_PQ_command_status
      end interface

      call c_char_to_f_string(c_PQ_command_status(pgresult), str)

      res => str
      
   end function PQcmdStatus


   function PQcmdTuples (pgresult) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      ! Input parameters
      type(c_ptr), intent(in) :: pgresult

      ! Output parameters
      character(:), pointer :: res

      ! ローカル変数の宣言
      character(:), allocatable, target, save :: str
      type(c_ptr), save :: cptr
      

      interface
         function c_PQ_command_tuples(pgresult) bind(c, name="PQcmdTuples")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            type(c_ptr) :: c_PQ_command_tuples
         end function c_PQ_command_tuples
      end interface

      cptr = c_PQ_command_tuples(pgresult)

      call c_char_to_f_string(cptr, str)

      call PQfreemem(cptr)

      res => str

   end function PQcmdTuples


   function PQoidValue(pgresult) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: unsigned

      type(c_ptr), intent(in) :: pgresult
      integer(int64) :: res

      interface
         function c_PQ_oid_value(pgresult) bind(c, name="PQoidValue")
            import c_ptr, uint32
            implicit none
            type(c_ptr), intent(in), value :: pgresult
            type(uint32) :: c_PQ_oid_value
         end function c_PQ_oid_value
      end interface

      res = int(c_PQ_oid_value(pgresult))

   end function PQoidValue
   

   !== Escaping Strings for Inclusion in SQL Commands

   function PQescapeLiteral (conn, str, length, errmsg) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      ! Input parameters
      type(c_ptr), intent(in) :: conn
      character(*),intent(in) :: str
      integer(c_size_t), intent(in) :: length
      character(*), intent(inout), optional :: errmsg

      ! Output variable
      character(:), pointer :: res

      ! Declare local variables
      integer(c_size_t) :: len                           ! Actual length of the input string
      character(:, kind=c_char), allocatable :: c_str    ! C-style string 
      character(:), allocatable, target :: result_string ! Target for the result string
      type(c_ptr) :: cptr                                ! Pointer to the result from the C func.
      
      ! Define an interface for the C func.
      interface
         function c_PQ_escape_literal (conn, str, length) bind(c, name="PQescapeLiteral")
            import c_ptr, c_size_t, c_char 
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: str
            integer(c_size_t), intent(in), value :: length
            type(c_ptr) :: c_PQ_escape_literal
         end function c_PQ_escape_literal
      end interface

      ! Initialize the result pointer variable
      res => null()

      ! Remove trailing spaces from the input string and assign it to c_str.
      ! Zero-type termination in not required for PQescapeLiteral.
      ! 引数の末尾の空白を除去して、c_strに代入する。PQescapeLiteralに限ってはゼロバイト終端は不要である。
      c_str = trim(str)

      ! Get the actual length of the string
      ! 実際の文字列の長さをローカル変数に取得する。
      len = len_trim(str)

      ! If the given length 'length' is too small, write a warging to 'errmsg'.
      ! 与えられた長さlengthが小さすぎる場合、errmsgに警告を書き込む。
      if (length < len) then
         if (present(errmsg)) then
            write(errmsg, *) "Warning: the length is too small for the string."
         end if
      end if 

      ! Call the C func. and assign the result's char ponter to 'cptr'
      ! C関数を呼び出し、結果のcharポインタをcptrに代入する。
      cptr = c_PQ_escape_literal(conn, c_str, length)

      ! if 'cptr' is a Null pointer, return to the caller. 
      ! In this case, the C func. leaves a message in 'conn' object.
      ! cptrがNullポインタの場合、呼び出し元に戻る。このときconnオブジェクトにメッセージを残す。
      if (.not. c_associated(cptr)) return 

      ! Store the result of the C func. in 'result_string'
      ! C関数の結果を文字列result_stringに格納する。
      call c_char_to_f_string(cptr, result_string)

      ! Associate the return value pointer.
      ! 戻り値のポインタを関連付ける。
      res => result_string

      ! Free the memory allocated in C.
      ! Cで割り付けられたメモリを解放する。
      call PQfreemem(cptr)

   end function PQescapeLiteral


   ! function PQescapeIdentifier
   ! function PQescapeStringConn
   ! function PQescapeByteConn
   ! function PQunescapeBytea


   !==================================================================!
   ! Asynchronous Command Processing

   function PQsendQuery (conn, command) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: command

      integer(int32) :: res
      character(:), allocatable :: c_command

      interface
         function c_PQ_send_query (conn, c_command) bind(c, name="PQsendQuery")
            import c_ptr, c_char, c_int
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: c_command(*)
            integer(c_int) :: c_PQ_send_query
         end function c_PQ_send_query
      end interface


      c_command = trim(command)//c_null_char

      res = c_PQ_send_query(conn, c_command)

   end function PQsendQuery


   ! function PQsendQueryParams
   ! function PQsendPrepare
   ! function PQsendQueryPrepared
   ! function PQsendDescribePrepared
   ! function PQsendDescribePortal

   function PQgetResult (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      type(c_ptr) :: res

      interface
         function c_PQ_get_result(conn) bind(c, name="PQgetResult")
            import c_ptr
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_get_result
         end function c_PQ_get_result
      end interface

      res = c_PQ_get_result(conn)
   end function PQgetResult
   
   
   function PQconsumeInput (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      Interface
         function c_PQ_consume_input (conn) bind(c, name="PQconsumeInput")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_consume_input
         end function c_PQ_consume_input
      end interface

      res = c_PQ_consume_input(conn)

   end function PQconsumeInput
      

   function PQisBusy(conn) result(isBusy)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      type(c_ptr), intent(in) :: conn
      integer(int32) :: res
      logical :: isBusy

      interface 
         function c_PQ_is_busy (conn) bind(c, name="PQisBusy")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_is_busy
         end function c_PQ_is_busy
      end interface

      isBusy = .false.

      res = c_PQ_is_busy(conn)

      if (res == 1) then
         isBusy = .true.
      else if (res == 0) then
         isBusy = .false.
      end if

   end function PQisBusy


   
   function PQsetnonblocking (conn, arg) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32), intent(in) :: arg

      integer(int32) :: res

      interface
         function c_PQ_set_nonblocking (conn, arg) bind(c, name="PQsetnonblocking")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int), intent(in), value :: arg
            integer(c_int) :: c_PQ_set_nonblocking
         end function c_PQ_set_nonblocking
      end interface

      res = c_PQ_set_nonblocking(conn, arg)

   end function PQsetnonblocking


   function PQisnonblocking (conn) result(isNonblocking)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res
      logical :: isNonblocking

      interface
         function c_PQ_is_nonblocking(conn) bind(c, name="PQisnonblocking")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_is_nonblocking
         end function c_PQ_is_nonblocking
      end interface

      res = c_PQ_is_nonblocking(conn)

      if (res == 1) then 
         isNonblocking = .true.
      else if (res == 0) then
         isNonblocking = .false. 
      end if 

   end function PQisnonblocking


   function PQflush (conn)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: PQflush

      interface
         function c_PQ_flush(conn) bind(c, name="PQflush")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_flush
         end function c_PQ_flush
      end interface

      PQflush = c_PQ_flush(conn)

   end function PQflush



   !=================================================================!
   ! Pipeline Mode
   
   ! function PQpipelineStatus
   ! function PQenterPipelineMode
   ! funciton PQexitPipelineMode
   ! function PQpipelineSync
   ! function PQsendFlushRequest
   

   !=================================================================!
   ! Functions Associated with the COPY Command
   
   ! function PQputCopyData
   ! function PQputCopyEnd
   ! function PQgetCopyData


   subroutine PQfreemem(cptr)
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: cptr

      interface
         subroutine c_PQ_free_memory (cptr) bind(c, name="PQfreemem")
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cptr
         end subroutine c_PQ_free_memory
      end interface

      call c_PQ_free_memory(cptr)

   end subroutine PQfreemem 


end module m_fe_exec