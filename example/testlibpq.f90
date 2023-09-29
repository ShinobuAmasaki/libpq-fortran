! example/testlibpq.f90
! 
! 
! Test the Fortran version of Libpq-Fortran,
!     the interface for the PostgreSQL libpq library.
! 
! The original is the example program 1 from the PostgreSQL official documentation.
! 
program main
   use :: libpq
   use, intrinsic :: iso_fortran_env, stdout=>output_unit, stderr=>error_unit
   use, intrinsic :: iso_c_binding

   character(:), allocatable :: conninfo
   type(c_ptr) :: conn
   type(c_ptr) :: res
   integer :: nFields
   integer :: i, j


   ! If the user supplies a parameter on the command line, use it as the
   ! conninfo string; otherwise default to setting dbname=postgres and using
   ! environment variables or defaults for all other connection parameters.

   integer(int32) :: argc
   type :: arguments
      character(:), allocatable :: v
   end type

   type(arguments), allocatable :: arg(:)

   ! Number of arguments not including execution command.
   argc = command_argument_count()

   ! String array for storing execution commands and arguments.
   allocate(arg(0:argc))

   if (argc > 1) then
      get_argument: block
         integer :: n, length_nth_arg
         do n = 0, argc

            ! get the length of the n-th argument.
            call get_command_argument(number=n, length=length_nth_arg)

            ! allocate a string with the same length as the n-th argument.
            allocate(character(length_nth_arg) :: arg(n)%v) 

            ! get the value of the n-th argument as a string.
            call get_command_argument(number=n, value=arg(n)%v)

         end do
      end block get_argument

      conninfo = arg(1)%v
   else
      conninfo = "dbname = postgres"
   end if

   ! Make a connection to the database.
   conn = PQconnectdb(conninfo)

   ! Check to see that the backend connection was successfully made.
   if (PQstatus(conn) /= CONNECTION_OK) then 
      write(stderr, *) PQerrorMessage(conn)
      call exit_nicely(conn)
   end if

   ! Set always-secure search path, so malicious users can't take control.
   res = PQexec(conn, "select pg_catalog.set_config('search_path', '', false)")

   if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      write(stderr, *) "SET failed: ", PQerrorMessage(conn)
      call PQclear(res)
      call exit_nicely(conn)
   end if
      
   ! Should PQclear type(c_ptr) result whenever it is no longer need to avoid
   ! memory leaks.
   call PQclear(res)

   ! Our test case here involve s using a cursor, for which we must be
   ! inside a transaction block.  We could do the whole thing with a
   ! PQexec() of "select * from pg_database", but that's too trivial
   ! to make a good example.

   ! Start a transaction block
   res = PQexec(conn, "BEGIN;")
   if (PQresultStatus(res) /= PGRES_COMMAND_OK) then
      write(stderr, *) "BEGIN command failed: ", PQerrorMessage(conn)
      call PQclear(res)
      call exit_nicely(conn)
   end if
   call PQclear(res)

   ! Fetch rows from pg_database, the system catalog of databases.
   res = PQexec(conn, "DECLARE myportal CURSOR FOR select * from pg_database")
   if (PQresultStatus(res) /= PGRES_COMMAND_OK) then
      write(stderr, *) "DECLARE CURSOR failed: ", PQerrorMessage(conn)
      call PQclear(res)
      call exit_nicely(conn)
   end if
   call PQclear(res)

   res = PQexec(conn, "FETCH ALL in myportal")
   if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      write(stderr, *) "FETCH ALL failed: ", PQerrorMessage(conn)
      call PQclear(res)
      call exit_nicely(conn)
   end if
   
   ! first, print out the attribute names
   nFields = PQnfields(res)
   do i = 0, nFields-1
      write(stdout, '(16(A10, 2x))', advance='no') PQfname(res, i)
   end do
   print *, ''

   ! next, print out the rows
   do i = 0, PQntuples(res)-1
      do j = 0, nFields-1
         write(stdout, '(16(a10, 2x))', advance='no') PQgetvalue(res, i, j)
      end do
      print *, ''
   end do
   call PQclear(res)

   ! close the portal ... we don't bother to check for errors ...
   res = PQexec(conn, "CLOSE myportal")
   call PQclear(res)

   ! end the transaction
   res = PQexec(conn, "END")
   call PQclear(res)

   ! close the connection to the database and cleanup
   call PQfinish(conn)
   stop

contains

   subroutine exit_nicely(conn)
      use :: libpq
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(inout) :: conn

      call PQfinish(conn)
      stop
   end subroutine exit_nicely

end program main
      