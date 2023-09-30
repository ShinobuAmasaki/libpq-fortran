module fe_exec_m
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
   public :: PQescapeIdentifier

   public :: PQexecParams

   interface PQexecParams
      module procedure :: PQexecParams_int32
      module procedure :: PQexecParams_int64
   end interface

   public :: PQprepare
   interface PQprepare
      module procedure :: PQprepare_int32
      module procedure :: PQprepare_int64
   end interface

   public :: PQexecPrepared
   public :: PQdescribePrepared
   public :: PQdescribePortal

   public :: PQsendPrepare
   interface PQsendPrepare
      module procedure :: PQsendPrepare_int32
      module procedure :: PQsendPrepare_int64
   end interface 

   public :: PQsendQueryPrepared
   interface PQsendQueryPrepared
      module procedure :: PQsendQueryPrepared_text
   end interface

   public :: PQsendDescribePrepared
   public :: PQsendDescribePortal
   public :: PQpipelineStatus
   public :: PQenterPipelineMode
   public :: PQexitPipelineMode
   public :: PQpipelineSync
   public :: PQsendFlushRequest
   public :: PQsetSingleRowMode

   public :: PQsendQueryParams
   interface PQsendQueryParams 
      module procedure :: PQsendQueryParams_int32
      module procedure :: PQsendQueryParams_int64
   end interface 

   public :: PQisthreadsafe

   public :: PQmakeEmptyPGresult
   public :: PQcopyResult
   public :: PQnotifies
      

contains


!==================================================================!
!== Command Execution Functions
!== Main Functions

   !> Send query to the server and wait until receiving the result.
   function PQexec (conn, query) result(res)
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

      !|> Submit a command to the server and waits for the result.
      ! > 
      ! > Returns a `PGresult` pointer or possibly a null pointer.
      ! > A non-null pointer will generally be returned except in out-of-memory conditions or
      ! > serious errors such as inability to send the command to the server.
      ! > The `[[PQresultStatus]]` function should be called to check the return value for any errors
      ! > (including the value of a null pointer, in which case it will return PGRES_FATAL_ERROR).
      ! > Use `[[PQerrorMessage]]` to get more information about such errors.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXEC)

      !*### Example
      !```Fortran
      ! type(c_ptr) :: conn, res
      ! integer :: stat 
      ! 
      ! conn = PQconnectdb("dbname=postgres")
      ! !...error handling...
      !
      ! res = PQexec(conn, "select 1234;")
      ! stat = PQresultStatus(res)
      ! if (stat /= PGRES_COMMAND_OK .and. stat /= PGRES_TUPLES_OK) then
      !    print *, PQerrorMessage(conn)
      !    call PQclear(res)
      !    call PQfinish(conn)
      !    error stop
      ! end if
      !
      ! ! ... some statements ...
      ! call PQclear(res)
      !```

   end function PQexec

   ! Only TEXT format for now 
   ! a version of int32 for paramTypes
   function PQexecParams_int32 (conn, command, nParams, paramTypes, paramValues) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned
      use :: character_operations_m
      implicit none

      !*> Submits a command to the server and waits for the result, with the ability to pass 
      ! > parameters separately from the SQL command text.
      ! > 
      ! > `PQexecParams` is like [`PQexec`](../proc/pqexec.html) but offers additional functionality:
      ! > parameter values can be specified separately from the command string proper, and query results
      ! > can be requested in text or binary format.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPARAMS)

      !!@note Binary format have not been implemented yet.

       !*### Example
      !```fortran
      ! block
      !    character(:), allocatable :: command
      !    character(8) :: values(2)
      !    type(c_ptr) :: res
      ! 
      !    command = "select $1::bigint + $2::bigint;"
      !    values(1) = "300"
      !    values(2) = "500"
      !
      !    res = PQexecParams(conn, command, size(values), [0, 0], values)
      !    if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      !       print *, PQerrorMessage(conn)
      !       call PQclear(res)
      !    end if
      !    
      !    print *, PQgetvalue(res, 0, 0) ! the result "800" is expected.
      !    call PQclear(res)
      !```
      
      ! Input parameters
      !> The connection object to send the command through. 
      type(c_ptr),    intent(in) :: conn
      
      !> The SQL command string to be executed. If parameters are used, 
      !> they are referred to in the command string as `$1`, `$2`, etc.
      character(*),   intent(in) :: command

      !> The number of parameters supplied; it is the length of the arrays `paramTypes`,
      !> `paramValues`. 
      integer(int32), intent(in) :: nParams

      !> Specifies, by OID, the data types to be assigned to the parameter symbols.
      !> If any particular element in the array is zero, the server infers a data type
      !> for the parameter symbol in the same way it would do for an untyped literal string.
      integer(int32), intent(in) :: paramTypes(:)

      !> Specifies the actual values of the parameters. A empty string in this array means
      !> the corresponding parameter is null. 
      character(*),   intent(in) :: paramValues(:)    ! paramValues(nParams)
      
      ! Output pgresult 
      type(c_ptr) :: res

      ! Local variables
      integer :: i
      integer(int64), allocatable :: paramTypes_i64(:)
      
      allocate(paramTypes_i64(size(paramTypes, dim=1)))

      do i = 1, size(paramTypes, dim=1)
         paramTypes_i64(i) = int(paramTypes(i), int64)
      end do

      res = PQexecParams(conn, command, nParams, paramTypes_i64, paramValues)

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPARAMS)
   end function PQexecParams_int32

   ! Only TEXT format for now 
   ! a version of int64 for paramTypes 
   function PQexecParams_int64 (conn, command, nParams, paramTypes, paramValues) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned
      use :: character_operations_m
      implicit none

       !*> Submits a command to the server and waits for the result, with the ability to pass 
      ! > parameters separately from the SQL command text.
      ! > 
      ! > `PQexecParams` is like [`PQexec`](../proc/pqexec.html) but offers additional functionality:
      ! > parameter values can be specified separately from the command string proper, and query results
      ! > can be requested in text or binary format.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPARAMS)

      !!@note Binary format have not been implemented yet.

      !*### Example
      !```fortran
      ! block
      !    character(:), allocatable :: command
      !    character(8) :: values(2)
      !    type(c_ptr) :: res
      ! 
      !    command = "select $1::bigint + $2::bigint;"
      !    values(1) = "300"
      !    values(2) = "500"
      !
      !    res = PQexecParams(conn, command, size(values), [0_8, 0_8], values)
      !    if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      !       print *, PQerrorMessage(conn)
      !       call PQclear(res)
      !    end if
      !    
      !    print *, PQgetvalue(res, 0, 0) ! the result "800" is expected.
      !    call PQclear(res)
      !```

      
      ! Input parameters
      !> The connection object to send the command through. 
      type(c_ptr),    intent(in) :: conn

      !> The SQL command string to be executed. If parameters are used, 
      !> they are referred to in the command string as `$1`, `$2`, etc.
      character(*),   intent(in) :: command

      !> The number of parameters supplied; it is the length of the arrays `paramTypes`,
      !> `paramValues`. 
      integer(int32), intent(in) :: nParams

      !> Specifies, by OID, the data types to be assigned to the parameter symbols.
      !> If any particular element in the array is zero, the server infers a data type
      !> for the parameter symbol in the same way it would do for an untyped literal string.
      integer(int64), intent(in) :: paramTypes(:)

      !> Specifies the actual values of the parameters. A empty string in this array means
      !> the corresponding parameter is null. 
      character(*),   intent(in) :: paramValues(:)    ! paramValues(nParams)

      ! Output pgresult 
      type(c_ptr) :: res

      ! Local variables
      character(:, kind=c_char), allocatable :: c_command
      type(uint32), allocatable, target :: c_paramtypes(:)      
      integer :: resultFormat
      integer :: max_len_val
      integer :: i

      ! For nParams >= 1
      interface
         function c_PQ_exec_parameters(conn, command, nParams, paramTypes, paramValues, &
                                       paramLengths, paramFormats, resultFormat) &
                     bind(c, name="PQexecParams")
            import c_ptr, c_int, uint32, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: command(*)      ! String
            integer(c_int), intent(in), value     :: nParams         ! Int scalar
            type(c_ptr), intent(in), value        :: paramTypes
            type(c_ptr), intent(in), value        :: paramValues    ! String array 
            type(c_ptr), intent(in), value        :: paramLengths    ! Int array
            type(c_ptr), intent(in), value        :: paramFormats    ! Int array
            integer(c_int), intent(in), value     :: resultFormat    ! Int scalar
            type(c_ptr) :: c_PQ_exec_parameters
         end function c_PQ_exec_parameters
      end interface


      resultFormat = 0 ! text format only for now.

      c_command =  trim(command)//c_null_char

      if (nParams >= 1) then

         max_len_val = max_length_char_array(paramValues)

         allocate(c_paramtypes(nParams))
         
         do i = 1, nParams
            c_paramtypes(i) = paramTypes(i)
         end do

         block
            character(max_len_val+1, kind=c_char), allocatable, target :: c_values(:)
            type(c_ptr), allocatable, target :: ptr_values(:)

            call cchar_array_from_strings_no_null(paramValues, c_values, max_len_val)
            call cptr_array_from_cchar_no_null(c_values, ptr_values)

            
            res = c_PQ_exec_parameters(&
                  conn, command, nParams, c_loc(c_paramTypes), &
                  c_loc(ptr_values(1)), c_null_ptr, c_null_ptr, resultFormat)

            ! for BINARY format
            ! res = c_PQ_exec_parameters(&
                  ! conn, command, nParams, c_paramTypes, &
                  ! ptr_values, paramLengths, paramFormats, resultFormat)


         end block 
         deallocate(c_paramtypes)
      ! for nParams == 0 
      else if (nParams == 0) then
         block
            type(c_ptr) :: null_paramTypes = c_null_ptr
            type(c_ptr) :: null_paramValues = c_null_ptr
            type(c_ptr) :: null_paramLength = c_null_ptr
            type(c_ptr) :: null_paramFormats = c_null_ptr
         
         res = c_PQ_exec_parameters(&
                        conn, command, nParams, &
                        null_paramTypes,   &
                        null_paramValues,  &
                        null_paramLength,  &
                        null_paramFormats, &
                        resultFormat)
         end block
      end if
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPARAMS)
   end function PQexecParams_int64

   ! --- PQprepare frontend
   function PQprepare_int32 (conn, stmtName, query, nParams, paramTypes) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned
      implicit none

      !*> Submits a request to create a prepared statement with the given parameters, and waits for completion.
      
      !*> `PQprepare` creates a prepared statement for later execution with `PQexecPrepared`.
      ! > This feature allows commands to be executed repeatedly without being parsed and
      ! > planned each time; see [PREPARE](https://www.postgresql.org/docs/current/sql-prepare.html) for details.

      !*> The function create a prepared statement named `stmtName` from the `query` string, which
      ! > must contain a single SQL command. `stmtName` can be `""` to create an unnamed statement,
      ! > in which case any pre-existing unnamed statement is automatically replaced; otherwise it is
      ! > an error if the statement name is already defined in the current session. If any parameters are used, 
      ! > they are referred to in the as `$1`, `$2`, etc. `nParams` is the number of parameters for which types
      ! > are pre-specified in the array `paramTypes`. 
      ! > `paramTypes` specifies, by OID, the data types to be assigned to the parameter symbols. 
      ! > If any particuler element in the `paramTypes` array is zero, the server assigns a data type to the
      ! > parameter symbol in the same way it would do for an untyped literal string. 
      ! > Also, the query can use parameter symbols with the numbers higher than `nParams`; data types will 
      ! > be inferred for these symbols as well. 
         ! (See [PQdescribePrepared](../proc/pqdescribeprepared.html)) for a means to find out what data types were inffered.) 
      
      !*> As with `[[PQexec]]`, the result is normally a `PGresult` object whose contents indicate server-side
      ! > success or failure. A null result indicates out-of-memory or inability to send to the command at all.
      ! > Use [`PQerrorMessage`](../proc/pqerrormessage.html) to get more information about such errors.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQPREPARE)


      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName
      character(*), intent(in) :: query
      integer(int32), intent(in) :: nParams
      integer(int32), intent(in) :: paramTypes(:)

      type(c_ptr) :: res

      type(uint32), allocatable :: u_paramTypes(:)
      integer :: i, siz

      siz = size(paramTypes, dim=1)
      allocate(u_paramTypes(siz)) 
   
      do i = 1, siz
         u_paramTypes(i) = paramTypes(i)
      end do

      res = PQprepare_back(conn, stmtName, query, nParams, u_paramTypes)

   end function PQprepare_int32


   function PQprepare_int64 (conn, stmtName, query, nParams, paramTypes) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned
      implicit none

      !*> Submits a request to create a prepared statement with the given parameters, and waits for completion.
      
      !*> `PQprepare` creates a prepared statement for later execution with `PQexecPrepared`.
      ! > This feature allows commands to be executed repeatedly without being parsed and
      ! > planned each time; see [PREPARE](https://www.postgresql.org/docs/current/sql-prepare.html) for details.

      !*> The function create a prepared statement named `stmtName` from the `query` string, which
      ! > must contain a single SQL command. `stmtName` can be `""` to create an unnamed statement,
      ! > in which case any pre-existing unnamed statement is automatically replaced; otherwise it is
      ! > an error if the statement name is already defined in the current session. If any parameters are used, 
      ! > they are referred to in the as `$1`, `$2`, etc. `nParams` is the number of parameters for which types
      ! > are pre-specified in the array `paramTypes`. 
      ! > `paramTypes` specifies, by OID, the data types to be assigned to the parameter symbols. 
      ! > If any particuler element in the `paramTypes` array is zero, the server assigns a data type to the
      ! > parameter symbol in the same way it would do for an untyped literal string. 
      ! > Also, the query can use parameter symbols with the numbers higher than `nParams`; data types will 
      ! > be inferred for these symbols as well. 
      
      ! (See [PQdescribePrepared](../proc/pqdescribeprepared.html)) for a means to find out what data types were inffered.) 
      
      !*> As with [`PQexec`](../proc/pqexec.html), the result is normally a `PGresult` object whose contents indicate server-side
      ! > success or failure. A null result indicates out-of-memory or inability to send to the command at all.
      ! > Use [`PQerrorMessage`](../proc/pqerrormessage.html) to get more information about such errors.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQPREPARE)

      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName
      character(*), intent(in) :: query
      integer(int32), intent(in) :: nParams
      integer(int64), intent(in) :: paramTypes(:)

      type(c_ptr) :: res

      type(uint32), allocatable :: u_paramTypes(:)
      integer :: i, siz

      siz = size(paramTypes, dim=1)
      allocate(u_paramTypes(siz))
   
      do i = 1, siz
         u_paramTypes(i) = paramTypes(i)
      end do

      res = PQprepare_back(conn, stmtName, query, nParams, u_paramTypes)
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQPREPARE)
   end function PQprepare_int64


   ! --- PQprepare backend
   function PQprepare_back (conn, stmtName, query, nParams, paramTypes) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned
      use :: character_operations_m
      implicit none

      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName
      character(*), intent(in) :: query
      integer(int32), intent(in) :: nParams
      type(uint32), intent(in) :: paramTypes(:)

      type(c_ptr) :: res

      character(:, kind=c_char), allocatable :: c_stmtName
      character(:, kind=c_char), allocatable :: c_query
      type(uint32), allocatable, target :: c_paramTypes(:)
      integer :: siz

      interface
         function c_PQ_prepare (conn, stmtName, query, nParams, paramTypes) bind(c, name="PQprepare")
            import c_ptr, uint32, c_char, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: stmtName(*)
            character(1, kind=c_char), intent(in) :: query(*)
            integer(c_int), intent(in), value :: nParams
            type(c_ptr), intent(in), value :: paramTypes
            type(c_ptr) :: c_PQ_prepare
         end function c_PQ_prepare
      end interface

      ! paramTypes(:)をc_paramTypesに複製してその先頭アドレスをc_PQ_prepareに渡すことを目的とする。
      siz = size(paramTypes, dim=1)
      allocate(c_paramTypes(siz))
      c_paramTypes(:) = paramTypes(:)

      ! Null終端の文字列を用意する
      c_stmtName = trim(adjustl(stmtName))//c_null_char
      c_query = trim(query)//c_null_char

      ! c_paramTypesのアドレスを渡す。
      res = c_PQ_prepare(conn, c_stmtName, c_query, nParams, c_loc(c_paramTypes))

   end function PQprepare_back


   function PQexecPrepared (conn, stmtName, nParams, paramValues) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: character_operations_m
      implicit none

      !*> Sends a request to execute a prepared statement with given parameters, and waits for the result.
      ! > 
      ! > `PQexecPrepared` is like `[[PQexecParam]]`, but the command to be executed is spcified by
      ! > naming a previously-prepared statement, instead of giving a query string. This feature allows 
      ! > commands that will be used repeated to be parsed and planned just once, rather than each
      ! > time they are executed. The statement must have been prepared previously in the current session.
      ! >
      ! > The parameters are identical to `[[PQexecParams]]`, except that the name of a prepared statement
      ! > is given instead of a query string, and the `paramTypes` parameter is not present (it is not
      ! > needed since the prepared statement's parameter types were determined when it was created).
      ! >
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPREPARED)
      
      ! Input paramters 
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName   ! statement name
      integer(int32), intent(in) :: nParams
      character(*), intent(in) :: paramValues(:)

      ! Output pointer
      type(c_ptr) :: res

      ! Local variables
      integer(int32) :: resultFormat, max_len_val
      character(:, kind=c_char), allocatable :: c_stmtName
      type(c_ptr) :: null_paramLengths = c_null_ptr
      type(c_ptr) :: null_paramFormats = c_null_ptr
      

      interface
         function c_PQ_exec_prepared (conn, stmtName, nParams, paramValues, paramLengths, paramFormats, resultFormat) &
            bind(c, name="PQexecPrepared")
            import c_ptr, c_int, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: stmtName(*)
            integer(c_int), intent(in), value     :: nParams
            type(c_ptr), intent(in), value        :: paramValues
            type(c_ptr), intent(in), value        :: paramLengths
            type(c_ptr), intent(in), value        :: paramFormats
            integer(c_int), intent(in), value     :: resultFormat
            type(c_ptr) :: c_PQ_exec_prepared
         end function c_PQ_exec_prepared
      end interface
      
      resultFormat = 0

      c_stmtName = trim(adjustl(stmtName))//c_null_char

      if (nParams >= 1) then
         max_len_val = max_length_char_array(paramValues)

         block
            character(max_len_val+1, kind=c_char), allocatable, target :: c_values(:)
            type(c_ptr), allocatable, target :: ptr_values(:)

            call cchar_array_from_strings_no_null(paramValues, c_values, max_len_val)
            call cptr_array_from_cchar_no_null(c_values, ptr_values)

            res = c_PQ_exec_prepared( &
                        conn, c_stmtName, nParams, &
                        c_loc(ptr_values(1)), &
                        null_paramLengths, &
                        null_paramFormats, &
                        resultFormat &
                  )
         end block

      else if (nParams == 0) then
         block
            type(c_ptr) :: null_paramValues = c_null_ptr

            res = c_PQ_exec_prepared( &
                        conn, c_stmtName, nParams, &
                        null_paramValues, &
                        null_paramLengths, &
                        null_paramFormats, &
                        resultFormat &
                  )
         end block
      end if 
   end function PQexecPrepared



   function PQdescribePrepared (conn, stmtName) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      !*> Submits a request to obtain information about the specified prepared statement, and waits for completion.

      !*> `PQdescribePrepared` allows an application to obtain information about a previously prepared statement.
      ! > 
      ! > `stmtName` can be `""` to reference the unnamed statement, otherwise it must be the name of an existing
      ! > prepared statement. On success, a `PGresult` with status `PGRES_COMMAND_OK` is returned. The `[[PQnparams]]`
      ! > and `[[PQparamtype]]` can be applied to this `PGresult` to obtain information about the parameters of the
      ! > prepared statement, and the functions `[[PQnfields]]`, `[[PQfname]]`, `[[PQftype]]` etc. provide information
      ! > about the result columns (if any) of the statement. 
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQDESCRIBEPREPARED)
      
      ! Input paramter
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName

      ! Output pointer
      type(c_ptr) :: res

      ! Local variable
      character(:, kind=c_char), allocatable :: c_stmtName

      interface
         function c_PQ_describe_prepared (conn, stmtName) bind(c, name="PQdescribePrepared")
            import c_ptr, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: stmtName(*)
            type(c_ptr) :: c_PQ_describe_prepared
         end function c_PQ_describe_prepared
      end interface

      c_stmtName = trim(adjustl(stmtName))//c_null_char

      res = c_PQ_describe_prepared(conn, c_stmtName)

   end function PQdescribePrepared


   function PQdescribePortal (conn, portalName) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      ! Input parameters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: portalName

      ! Output parameter
      type(c_ptr) :: res

      ! Local variables
      character(:, kind=c_char), allocatable :: c_portalName

      interface
         function c_PQ_describe_portal(conn, portalName) bind(c, name="PQdescribePortal")
            import c_ptr, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: portalName(*)
            type(c_ptr) :: c_PQ_describe_portal
         end function c_PQ_describe_portal
      end interface

      c_portalName = trim(adjustl(portalName))//c_null_char

      res = c_PQ_describe_portal(conn, c_portalName)
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQDESCRIBEPORTAL)
   end function PQdescribePortal


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

      !*> Returns the result status of the command.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTSTATUS)
      
   end function PQresultStatus


   function PQresStatus(status) result(res)
      use :: character_operations_m
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESSTATUS)
   end function PQresStatus


   function PQresultErrorMessage(pgresult) result(res)
      use :: character_operations_m
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTERRORMESSAGE)
   end function PQresultErrorMessage


   function PQresultVerboseErrorMessage(pgresult, verbosity, show_context) result(res)
      use :: character_operations_m
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTVERBOSEERRORMESSAGE)
   end function PQresultVerboseErrorMessage


   function PQresultErrorField(pgresult, fieldcode) result(res)
      use :: character_operations_m
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTERRORFIELD)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQCLEAR)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQNTUPLES)
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
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQNFIELDS)
   end function PQnfields


   function PQfname(pgresult, field_num) result(res)
      use :: character_operations_m
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFNAME)
   end function PQfname

   
   function PQfnumber (pgresult, column_name)
      use :: character_operations_m
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
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFNUMBER)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFTABLE)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFTABLECOL)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFFORMAT)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFTYPE)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFMOD)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFSIZE)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQBINARYTUPLES)
   end function PQbinaryTuples

  

   function PQgetvalue (pgresult, tuple_num, field_num)
      use :: character_operations_m
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQGETVALUE)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQGETISNULL)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQGETLENGTH)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQNPARAMS)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQPARAMTYPE)
   end function PQparamtype
      
   

!== Retrieving Other Result Information

   function PQcmdStatus(pgresult) result(res)
      use :: character_operations_m
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
      
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQCMDSTATUS)
   end function PQcmdStatus


   function PQcmdTuples (pgresult) result(res)
      use :: character_operations_m
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

      res => str

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQCMDTUPLES)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQOIDVALUE)
   end function PQoidValue
   

!== Escaping Strings for Inclusion in SQL Commands

   function PQescapeLiteral (conn, str, length, errmsg) result(res)
      use :: character_operations_m
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
      character(:), allocatable, target, save :: result_string ! Target for the result string
      type(c_ptr) :: cptr                                ! Pointer to the result from the C func.
      
      ! Define an interface for the C func.
      interface
         function c_PQ_escape_literal (conn, str, length) bind(c, name="PQescapeLiteral")
            import c_ptr, c_size_t, c_char 
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: str(*)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQESCAPELITERAL)
   end function PQescapeLiteral


   ! この関数の構造は上のPQescapeLiteralとほとんど同じなので、それに含まれるコメントも参照されたい
   ! The structure of this function closely resembles that of the `PQescapeLiteral`,
   ! and the comments included in it should also be referenced.
   function PQescapeIdentifier (conn, str, length, errmsg) result(res)
      use :: character_operations_m
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env

      ! Input parameters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: str
      integer(c_size_t), intent(in) :: length
      character(*), intent(inout), optional :: errmsg

      ! Output variable
      character(:), pointer :: res

      ! Declare local variable
      integer(c_size_t) :: len
      character(:, kind=c_char), allocatable :: c_str
      character(:), allocatable, target, save :: result_string
      type(c_ptr) :: cptr

      ! Define an interface for the C func.
      interface
         function c_PQ_escape_identifier(conn, str, length) bind(c, name="PQescapeIdentifier")
            import c_ptr, c_size_t, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: str(*)
            integer(c_size_t), intent(in), value :: length
            type(c_ptr) :: c_PQ_escape_identifier
         end function c_PQ_escape_identifier
      end interface

      res => null()

      c_str = trim(str)

      len = len_trim(str)

      if (length < len) then
         if (present(errmsg)) then
            write(errmsg, *) "Warning: the length is too small for the string."
         end if
      end if

      cptr = c_PQ_escape_identifier(conn, c_str, length)

      if (.not. c_associated(cptr)) return

      call c_char_to_f_string(cptr, result_string)

      res => result_string 
      
      call PQfreemem(cptr)

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQESCAPEIDENTIFIER)
   end function PQescapeIdentifier

   ! function PQescapeStringConn

   ! function PQescapeByteConn
   ! function PQunescapeBytea


!=================================================================!
!== Asynchronous Command Processing

   !|> Submits a command to the server without waiting for the result(s).
   ! > `1` is returned if the command was successfully dispatched and
   ! > `0` if not (in which case, use PQerrorMessage to get more information about the failure).
   !
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

      !*> After successfully calling `PQsendQuery`, call `[[PQgetResult]]` one or more times
      ! > to obtain the results. `PQsendQuery` cannot be called again (on the same connection)
      ! > until `[[PQgetResult]]` has returned a null pointer, indicating that the command is done.

      !*> In pipeline mode, this function is disallowed.
      ! > 
      ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDQUERY)

      !*### Example
      !```Fortran
      !   type(c_ptr) :: conn
      !   type(c_ptr) :: res
      !   character(:), allocatable :: command
      !   integer :: ires
      !
      !   conn = PQconnectdb("dbname=postgres")
      !   (...error handling...)
      !
      !   ires = PQsendQuery(conn, command)
      !   if (ires /= 1) then
      !      print *, PQerrorMessage(conn)
      !   end if
      !
      !   res = PQgetResult(conn)
      !
      !   do while (c_associated(res))
      !
      !      if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      !         print *, PQerrorMessage(conn)
      !      end if
      !
      !      print *, PQgetvalue(res, 0, 0)
      !      call PQclear(res)
      !      
      !      res = PQgetResult(conn)
      !
      !   end do
      !```

   end function PQsendQuery

   ! 戻り値はinteger(4)なのでPQexecParamsとは別に実装する
   ! This function sends a prepared SQL query to a PostgreSQL connection asynchronously,
   ! separate from PQexecParams.
   function PQsendQueryParams_back (conn, command, nParams, paramTypes, paramValues) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: unsigned
      use :: character_operations_m

      ! Input parameters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: command
      integer(int32), intent(in) :: nParams

      type(uint32), intent(in) :: paramTypes(:)
      character(*), intent(in) :: paramValues(:)

      ! Output integer
      integer(int32) :: res

      ! Local variables
      character(:, kind=c_char), allocatable :: c_command
      type(uint32), allocatable, target :: c_paramTypes(:)
      integer(int32) :: resultFormat
      integer(int32) :: max_len_val
      type(c_ptr) :: cptr_paramTypes
      type(c_ptr) :: null_paramLengths = c_null_ptr
      type(c_ptr) :: null_paramFormats = c_null_ptr

      ! For nParams >= 1
      interface
         function c_PQ_send_query_parameters(conn, command, nParams, paramTypes, paramValues, &
                     paramLengths, paramFormats, resultFormat) &
                     bind(c, name="PQsendQueryParams")
            import c_ptr, c_int, uint32, c_char
            implicit none
            type(c_ptr),    intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: command(*)
            integer(c_int), intent(in), value :: nParams
            type(c_ptr),    intent(in), value :: paramTypes
            type(c_ptr),    intent(in), value :: paramValues
            type(c_ptr),    intent(in), value :: paramLengths
            type(c_ptr),    intent(in), value :: paramFormats
            integer(c_int), intent(in), value :: resultFormat
            integer(c_int) :: c_PQ_send_query_parameters
         end function c_PQ_send_query_parameters
      end interface 

      ! For nParams == 0
      interface
         function c_PQ_send_query_parameters_zero(conn, command, nParams, paramTypes, paramValues,&
                     paramLengths, paramFormats, resultFormat) &
                     bind(c, name="PQsendQueryParams")
            import c_ptr, c_int, uint32, c_char
            implicit none
            type(c_ptr),    intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: command(*)
            integer(c_int), intent(in), value :: nParams
            type(c_ptr),    intent(in), value :: paramTypes
            type(c_ptr),    intent(in), value :: paramValues
            type(c_ptr),    intent(in), value :: paramLengths
            type(c_ptr),    intent(in), value :: paramFormats
            integer(c_int), intent(in), value :: resultFormat
            integer(c_int) :: c_PQ_send_query_parameters_zero
         end function c_PQ_send_query_parameters_zero
      end interface 

      resultFormat = 0 ! text format only for now.

      c_command =  trim(command)//c_null_char

      if (nParams >= 1) then

         max_len_val = max_length_char_array(paramValues)

         allocate(c_paramTypes(nParams))

         c_paramTypes(:) = paramTypes(:)

         cptr_paramTypes = c_loc(c_paramTypes)

         block
            character(max_len_val+1, kind=c_char), allocatable, target :: c_values(:)
            type(c_ptr), allocatable :: ptr_values(:)
            

            call cchar_array_from_strings_no_null(paramValues, c_values, max_len_val)
            call cptr_array_from_cchar_no_null(c_values, ptr_values)

            res = c_PQ_send_query_parameters(&
                     conn, c_command, nParams, &
                     cptr_paramTypes, &
                     ptr_values, &
                     null_paramLengths, &
                     null_paramFormats, &
                     resultFormat &
                     )
         end block
      
      else if (nParams == 0) then
         block
            type(c_ptr) :: null_paramTypes = c_null_ptr
            type(c_ptr) :: null_paramValues = c_null_ptr
            
            res = c_PQ_send_query_parameters_zero( &
                     conn, c_command, nParams, &
                     null_paramTypes, &
                     null_paramValues, &
                     null_paramLengths, &
                     null_paramFormats, &
                     resultFormat &
                  )

         end block
      end if

   end function PQsendQueryParams_back


   !>> Submit a command and separate parameter to the server without waiting for the result(s).
   function PQsendQueryParams_int32 (conn, command, nParams, paramTypes, paramValues) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned

      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: command
      integer(int32), intent(in) :: nParams

      integer(int32), intent(in) :: paramTypes(:)
      character(*), intent(in) :: paramValues(:)

      integer(int32) :: res

      integer :: i, siz
      type(uint32), allocatable :: u_paramTypes(:)
      
      siz = size(paramTypes, dim=1)

      allocate(u_paramTypes(siz))

      do i = 1, siz
         u_paramTypes(i) = paramTypes(i)
      end do

      res = PQsendQueryParams_back(conn, command, nParams, u_paramTypes, paramValues)

      !*> This is equivalent to [`PQsendQuery`](../proc/pqsendquery.html), except that query parameters can be specified
      ! > separately form the query string. 
      ! > This function's parameters are handled identically [`PQexecParams`](../interface/pqexecparams.html).
      ! > Like [`PQexecParams`](../interface/pqexecparams.html), it allows only one command in the query.
      ! 
      ! >  cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDQUERYPARAMS)
   end function PQsendQueryParams_int32

   !>> Submit a command and separate parameter to the server without waiting for the result(s).
   function PQsendQueryParams_int64 (conn, command, nParams, paramTypes, paramValues) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned

      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: command
      integer(int32), intent(in) :: nParams

      integer(int64), intent(in) :: paramTypes(:)
      character(*), intent(in) :: paramValues(:)

      integer(int32) :: res

      integer :: i, siz
      type(uint32), allocatable :: u_paramTypes(:)
      
      siz = size(paramTypes, dim=1)

      allocate(u_paramTypes(siz))

      do i = 1, siz
         u_paramTypes(i) = paramTypes(i)
      end do

      res = PQsendQueryParams_back(conn, command, nParams, u_paramTypes, paramValues)

      !*> This is equivalent to [`PQsendQuery`](../proc/pqsendquery.html), 
      ! > except that query parameters can be specified
      ! > separately form the query string. 
      ! > This function's parameters are handled identically [`PQexecParams`](../interface/pqexecparams.html).
      ! > Like [`PQexecParams`](../interface/pqexecparams.html), it allows only one command in the query.
      ! 
      ! >  cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDQUERYPARAMS)
   end function PQsendQueryParams_int64

      
   ! 戻り値はinteger(4)なのでPQprepareとは別に実装する
   !> This function sends a prepared SQL query to a PostgreSQL connection asynchronously,
   !> separate from PQprepare.
   ! Inputs:
   !  - conn: PostgreSQL connection pointer
   !  - stmtName: Name for prepared statement
   !  - query: SQL query string
   !  - nParams : Number of parameters in the query
   !  - paramTypes: Array of parameter types
   ! Outputs:
   !  - res: Result code (integer, 1 or 0)
   !
   function PQsendPrepare_back (conn, stmtName, query, nParams, paramTypes) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: unsigned
      use :: character_operations_m
      implicit none

      ! Input parameters
      type(c_ptr), intent(in) :: conn
      character(*),intent(in) :: stmtName
      character(*), intent(in) :: query
      integer(int32), intent(in) :: nParams
      type(uint32), intent(in) :: paramTypes(:)

      ! Output variable
      integer(int32) :: res      

      character(:, kind=c_char), allocatable :: c_stmtName
      character(:, kind=c_char), allocatable :: c_query
      type(uint32), allocatable, target :: c_paramTypes(:)
      integer :: siz

      interface
         function c_PQ_send_prepare (conn, stmtName, query, nParams, paramTypes) bind(c, name="PQsendPrepare")
            import c_ptr, uint32, c_char, c_int
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: stmtName(*)
            character(1, kind=c_char), intent(in) :: query(*)
            integer(c_int), intent(in), value :: nParams
            type(c_ptr), intent(in), value :: paramTypes 
            integer(c_int) :: c_PQ_send_prepare
         end function c_PQ_send_prepare
      end interface

      ! paramTypes(:)をc_paramTypesに複製してその先頭アドレスをc_PQ_send_prepareに渡すことを目的とする。
      siz = size(paramTypes, dim=1)
      allocate(c_paramTypes(siz))
      c_paramTypes(:) = paramTypes(:)

      ! Null終端の文字列を用意する
      c_stmtName = trim(adjustl(stmtName))//c_null_char
      c_query = trim(query)//c_null_char

      ! c_paramTypesのアドレスを渡し、戻り値は
      res = c_PQ_send_prepare(conn, c_stmtName, c_query, nParams, c_loc(c_paramTypes))
      
   end function PQsendPrepare_back

   !|> Sends a request to create a prepared statement with the given parameters, without waiting for completion.
   ! >
   ! > This is an asynchronous version of `PQprepare`:
   ! > it returns `1` if it was able to dispatch the request, and `0` if not.
   ! > After a successfull call, call [`PQgetResult`](../proc/pqgetresult.html) to determine whether the server successfully created the prepared statement. 
   ! > The function's parameters are handled identically to `PQprepare`.
   ! > 
   ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDPREPARE)
   function PQsendPrepare_int32(conn, stmtName, query, nParams, paramTypes) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: unsigned
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName
      character(*), intent(in) :: query
      integer(int32), intent(in) :: nParams
      integer(int32), intent(in) :: paramTypes(:)

      integer(int32) :: res

      type(uint32), allocatable :: u_paramTypes(:)
      integer :: i, siz

      siz = size(paramTypes, dim=1)
      allocate(u_paramTypes(siz))

      do i = 1, siz
         u_paramTypes(i) = paramTypes(i)
      end do

      res = PQsendPrepare_back(conn, stmtName, query, nParams, u_paramTypes)


   end function PQsendPrepare_int32

   !|> Sends a request to create a prepared statement with the given parameters, without waiting for completion.
   ! >
   ! > This is an asynchronous version of `PQprepare`:
   ! > it returns `1` if it was able to dispatch the request, and `0` if not.
   ! > After a successfull call, call [`PQgetResult`](../proc/pqgetresult.html) to determine whether the server successfully created the prepared statement. 
   ! > The function's parameters are handled identically to `PQprepare`.
   ! > 
   ! > cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDPREPARE)
   function PQsendPrepare_int64(conn, stmtName, query, nParams, paramTypes) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      use :: unsigned
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName
      character(*), intent(in) :: query
      integer(int32), intent(in) :: nParams
      integer(int64), intent(in) :: paramTypes(:)

      integer(int32) :: res

      type(uint32), allocatable :: u_paramTypes(:)
      integer :: i, siz

      siz = size(paramTypes, dim=1)
      allocate(u_paramTypes(siz))

      do i = 1, siz
         u_paramTypes(i) = int(paramTypes(i))
      end do

      res = PQsendPrepare_back(conn, stmtName, query, nParams, u_paramTypes)
   
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDPREPARE)
   end function PQsendPrepare_int64


   function PQsendQueryPrepared_text (conn, stmtName, nParams, paramValues) result(res)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      use :: character_operations_m

      ! Input paramters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName
      integer(int32), intent(in) :: nParams
      character(*), intent(in) :: paramValues(:)

      ! Output integer
      integer(int32) :: res

      ! Local variales
      integer(int32) :: resultFormat, max_len_val
      character(:, kind=c_char), allocatable :: c_stmtName
      type(c_ptr) :: null_paramLengths = c_null_ptr
      type(c_ptr) :: null_paramFormats = c_null_ptr

      interface
         function c_PQ_send_query_prepared (conn, stmtName, nParams, paramValues, paramLengths, paramFormats, resultFormat) &
                     bind(c, name="PQsendQueryPrepared")
            import c_ptr, c_int, c_char
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: stmtName(*)
            integer(c_int), intent(in), value     :: nParams
            type(c_ptr), intent(in)               :: paramValues(:)
            type(c_ptr), intent(in), value        :: paramLengths
            type(c_ptr), intent(in), value        :: paramFormats
            integer(c_int), intent(in), value     :: resultFormat
            integer(c_int) :: c_PQ_send_query_prepared
         end function c_PQ_send_query_prepared
      end interface

      interface
         function c_PQ_send_query_prepared_zero (conn, stmtName, nParams, paramValues, paramLengths, paramFormats, resultFormat) &
                     bind(c, name="PQsendQueryPrepared")
            import c_ptr, c_int, c_char
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: stmtName(*)
            integer(c_int), intent(in), value     :: nParams
            type(c_ptr), intent(in), value        :: paramValues
            type(c_ptr), intent(in), value        :: paramLengths
            type(c_ptr), intent(in), value        :: paramFormats
            integer(c_int), intent(in), value     :: resultFormat
            integer(c_int) :: c_PQ_send_query_prepared_zero
         end function c_PQ_send_query_prepared_zero
      end interface
         
      resultFormat = 0
      res = 0

      c_stmtName = trim(adjustl(stmtName))//c_null_char

      if (nParams >= 1) then
         max_len_val = max_length_char_array(paramValues)

         block
            character(max_len_val+1, kind=c_char), allocatable, target :: c_values(:)
            type(c_ptr), allocatable :: ptr_values(:)

            call cchar_array_from_strings_no_null(paramValues, c_values, max_len_val)
            call cptr_array_from_cchar_no_null(c_values, ptr_values)

            res = c_PQ_send_query_prepared( &
                     conn, c_stmtName, nParams, &
                     ptr_values, &
                     null_paramLengths, &
                     null_paramFormats, &
                     resultFormat &
                  )
         end block

      else if (nParams == 0 ) then
         block
            type(c_ptr) :: null_paramValues = c_null_ptr

            res = c_PQ_send_query_prepared_zero( &
                     conn, c_stmtName, nParams, &
                     null_paramValues,  &
                     null_paramLengths, &
                     null_paramFormats, &
                     resultFormat &
                  )
         end block
      end if 

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDQUERYPREPARED)
   end function PQsendQueryPrepared_text


   function PQsendDescribePrepared (conn, stmtName) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      ! Input paramters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: stmtName

      ! Output integer
      integer(int32) :: res

      ! Local variable
      character(:, kind=c_char), allocatable :: c_stmtName

      interface
         function c_PQ_send_describe_prepared (conn, stmtName) bind(c, name="PQsendDescribePrepared")
            import c_ptr, c_char, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char) :: stmtName(*)
            integer(c_int) :: c_PQ_send_describe_prepared
         end function c_PQ_send_describe_prepared
      end interface

      ! Initialize the result variable
      res = 1

      ! Trim and left-align 'stmtName', then append a null termination character.
      c_stmtName = trim(adjustl(stmtName))//c_null_char

       ! Call the C function 'PQsendDescribePrepared' with the converted stmtName.
      res = c_PQ_send_describe_prepared(conn, c_stmtName)


   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDDESCRIBEPREPARED)
   end function PQsendDescribePrepared


   function PQsendDescribePortal (conn, portalName) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      ! Input paramters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: portalName

      ! Output integer
      integer(int32) :: res

      ! Local variable
      character(:, kind=c_char), allocatable :: c_portalName

      interface
         function c_PQ_send_describe_portal (conn, portalName) bind(c, name="PQsendDescribePortal")
            import c_ptr, c_char, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char) :: portalName(*)
            integer(c_int) :: c_PQ_send_describe_portal
         end function c_PQ_send_describe_portal
      end interface

      ! Initialize the result variable
      res = 1

      ! Trim and left-align 'portalName', then append a null termination character.
      c_portalName = trim(adjustl(portalName))//c_null_char

      ! Call the C function 'PQsendDescribePortral' with the converted portralName.
      res = c_PQ_send_describe_portal(conn, c_portalName)

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSENDDESCRIBEPORTAL)
   end function PQsendDescribePortal


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
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQGETRESULT)
   end function PQgetResult
   
   !> If input is available from the server, consume it.
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQCONSUMEINPUT)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQISBUSY)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQSETNONBLOCKING)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQISNONBLOCKING)
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-async.html#LIBPQ-PQFLUSH)
   end function PQflush



!=================================================================!
!== Pipeline Mode
   
   function PQpipelineStatus (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none

      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      interface
         function c_PQ_pipeline_status (conn) bind(c, name="PQpipelineStatus")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_pipeline_status
         end function c_PQ_pipeline_status
      end interface
      
      res = c_PQ_pipeline_status(conn)
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PQPIPELINESTATUS)
   end function PQpipelineStatus


   function PQenterPipelineMode (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      interface
         function c_PQ_enter_pipeline_mode (conn) bind(c, name="PQenterPipelineMode")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_enter_pipeline_mode
         end function c_PQ_enter_pipeline_mode
      end interface 

      res = c_PQ_enter_pipeline_mode(conn)

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PQPIPELINEMODE)
   end function PQenterPipelineMode


   function PQexitPipelineMode (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      interface
         function c_PQ_exit_pipeline_mode (conn) bind(c, name="PQexitPipelineMode")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_exit_pipeline_mode
         end function c_PQ_exit_pipeline_mode
      end interface 

      res = c_PQ_exit_pipeline_mode(conn)
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PQEXITPIPELINEMODE)
   end function PQexitPipelineMode


   function PQpipelineSync (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      interface
         function c_PQ_pipeline_sync (conn) bind(c, name="PQpipelineSync")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_pipeline_sync
         end function c_PQ_pipeline_sync
      end interface 

      res = c_PQ_pipeline_sync(conn)
      !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PQPIPELINESYNC)
   end function PQpipelineSync


   function PQsendFlushRequest (conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      interface
         function c_PQ_send_flush_request (conn) bind(c, name="PQsendFlushRequest")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_send_flush_request
         end function c_PQ_send_flush_request
      end interface 

      res = c_PQ_send_flush_request(conn)

      !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PQSENDFLUSHREQUEST)
   end function PQsendFlushRequest



!=================================================================!
!== Retrieving Query Result Row-by-Row
   function PQsetSingleRowMode(conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res

      interface
         function c_PQ_set_single_row_mode (conn) bind(c, name="PQsetSingleRowMode")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: c_PQ_set_single_row_mode
         end function c_PQ_set_single_row_mode
      end interface

      res = c_PQ_set_single_row_mode(conn)
   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-single-row-mode.html#LIBPQ-PQSETSINGLEROWMODE)   
   end function PQsetSingleRowMode

!=================================================================!
!== Asynchronous Notification
   
   function PQnotifies(conn) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      type(c_ptr) :: res

      interface
         function c_PQ_notifies (conn) bind(c, name="PQnotifies")
            import 
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_notifies
         end function c_PQ_notifies
      end interface

      res = c_PQ_notifies(conn)

   end function PQnotifies



!=================================================================!
!== Functions Associated with the COPY Command
   
   ! function PQputCopyData
   ! function PQputCopyEnd
   ! function PQgetCopyData

!=================================================================!
!== Miscellaneous Functions
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

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-misc.html#LIBPQ-PQFREEMEM)
   end subroutine PQfreemem


   function PQmakeEmptyPGresult (conn, status) result(res)
      use :: enumerators_t
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      integer(int32), intent(in) :: status

      type(c_ptr) :: res

      interface
         function c_PQ_make_empty_PGresult (conn, status) bind(c, name="PQmakeEmptyPGresult")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int), intent(in), value :: status
            type(c_ptr) :: c_PQ_make_empty_PGresult
         end function
      end interface

      if ( 0 <= status .and. status <= PGRES_PIPELINE_ABORTED) then
         res = c_PQ_make_empty_PGresult(conn, status)
      else
         res = c_null_ptr
      end if

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-misc.html#LIBPQ-PQMAKEEMPTYRESULT)
   end function PQmakeEmptyPGresult


   function PQcopyResult(conn, res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      
      type(c_ptr), intent(in) :: conn
      type(c_ptr), intent(in) :: res

      type(c_ptr) :: PQcopyResult

      interface
         function c_PQ_copy_result(conn, res) bind(c, name="PQcopyResult")
            import c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: res
            type(c_ptr) :: c_PQ_copy_result
         end function c_PQ_copy_result
      end interface

      PQcopyResult = c_PQ_copy_result(conn, res)

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-misc.html#LIBPQ-PQCOPYRESULT)
   end function PQcopyResult


   !> Get the threadsafety status of the current running libpq library.
   function PQisthreadsafe()
      use, intrinsic :: iso_c_binding
      implicit none
      
      integer :: result
      logical :: PQisthreadsafe

      interface
         function c_PQ_is_threadsafe() bind(c, name="PQisthreadsafe")
            import c_int
            implicit none
            integer(c_int) :: c_PQ_is_threadsafe
         end function c_PQ_is_threadsafe
      end interface

      result = c_PQ_is_threadsafe()

      if (result == 1) then
         PQisthreadsafe = .true.
      else
         PQisthreadsafe = .false.
      end if

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-threading.html#LIBPQ-PQISTHREADSAFE)
   end function PQisthreadsafe


end module fe_exec_m