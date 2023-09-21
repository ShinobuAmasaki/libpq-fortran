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


   ! function PQresultErrorField


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

   ! function PQcmdStatus
   ! function PQcmdTuples
   ! function PQoidValue
   ! function PQoidStatus
   
   
   !== Escaping Strings for Inclusion in SQL Commands

   ! function PQescapeLiteral
   ! function PQescapeIdentifier
   ! function PQescapeStringConn
   ! function PQescapeByteConn
   ! function PQunescapeBytea


   !==================================================================!
   ! Asynchronous Command Processing

   ! function PQsendQuery
   ! function PQsendQueryParams
   ! function PQsendPrepare
   ! function PQsendQueryPrepared
   ! function PQsendDescribePrepared
   ! function PQsendDescribePortal
   ! function PQgetResult
   ! function PQconsumeInput
   ! function PQisBusy
   ! function PQsetnonblocking
   ! funciton PQisnonblocking
   ! function PQflush


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