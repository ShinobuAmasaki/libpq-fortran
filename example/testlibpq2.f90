! example/testlibpq2.f90
!
! Test of the asynchronous notification interface on Linux
!
! The original is the example program 2 from the PostgreSQL official documentation.
!
! Start this program, then from psql in another window do
!    NOTIFY TBL2;
! Repeat four times to get this program to exit.
!
! Or, if you want to get fancy, try this:
! populate a database with the following commands
! 
!   CREATE SCHEMA TESTLIBPQ2;
!   SET search_path = TESTLIBPQ2;
!   CREATE TABLE TBL1 (i int4);
!   CREATE TABLE TBL2 (i int4);
!   CREATE ROLE r1 AS ON INSERT TO TBL1 DO
!      (INSERT INTO TBL2 VALUES (new.i); NOTIFY TBL2);

program main

   use :: libpq
   use :: unsigned
   use, intrinsic :: iso_fortran_env, stdout=>output_unit, stderr=>error_unit
   use, intrinsic :: iso_c_binding
   implicit none
   
   character(:), allocatable :: conninfo
   type(c_ptr) :: conn, res
   type(c_ptr) :: notify
   integer(int32) :: nnotifies

   ! If the user supplies a parameter on the command line, use it as the
   ! conninfo string; otherwise default to setting dbname=postgres and using
   ! environment variables or defaults for all other connection parameters
   
   integer(int32) :: argc
   type :: arguments
      character(:), allocatable :: v
   end type
   type(arguments), allocatable :: arg(:)

   ! for Linux
   type, bind(c) :: fd_set
      integer(c_int64_t) :: fds_bits(16)
   end type

   interface
      function select(nfds, readfds, writefds, exceptfds, timeout) bind(c)
         import c_ptr, c_int
         implicit none
         integer(c_int), intent(in), value :: nfds
         type(c_ptr), intent(in), value :: readfds
         type(c_ptr), intent(in), value :: writefds
         type(c_ptr), intent(in), value :: exceptfds
         type(c_ptr), intent(in), value :: timeout
         integer(c_int) :: select
      end function select
   end interface

   ! Interface for using macro FD_ZERO.
   interface
      subroutine c_FD_ZERO(set) bind(c, name="fd_zero_wrap")
         import fd_set
         implicit none
         type(fd_set), intent(inout) :: set
      end subroutine c_FD_ZERO
   end interface 

   ! Interface for using macro FD_SET
   interface
      subroutine c_FD_SET(fd, set) bind(c, name="fd_set_wrap")
         import fd_set, c_int
         implicit none
         integer(c_int), intent(in), value :: fd
         type(fd_set), intent(inout) :: set
      end subroutine c_FD_SET
   end interface
         

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

   ! Issue LISTEN command to enable notifications from the rule's NOTIFY.
   res = PQexec(conn, "LISTEN TBL2")
   if (PQresultStatus(res) /= PGRES_COMMAND_OK) then
      write(stderr, *) "LISTEN command failed: ", PQerrorMessage(conn)
      call PQclear(res)
      call exit_nicely(conn)
   end if
   call PQclear(res)

   ! Quit after four notifies are recieved.
   nnotifies = 0
   do while (nnotifies < 4)

      ! Sleep until something happens on the connection.
      ! We use select(2) to wait for input, but you could also use poll() or
      ! similar facilities.

      block
         integer :: sock
         integer :: res
         type(fd_set), target :: input_mask
         type(pgNotify), pointer :: fptr
         character(4), pointer :: str_ptr


         sock = PQsocket(conn)

         if (sock < 0) exit  ! shouldn't happen

         call c_FD_ZERO(input_mask)
         call c_FD_SET(sock, input_mask)

         if (select(sock+1, c_loc(input_mask), c_null_ptr, c_null_ptr, c_null_ptr) < 0) then

            write(stderr, *) "select() faild."
            call exit_nicely(conn)

         end if 

         print *, "PQisBusy: ", PQisBusy(conn)

         ! Now check for input.
         res = PQconsumeInput(conn)
         print *, "PQconsumeInput"
         print *, "PQisBusy: ", PQisBusy(conn)

         notify = PQnotifies(conn)

         do while (c_associated(notify))
            call c_f_pointer(notify, fptr)
            call c_f_pointer(fptr%relname, str_ptr)
            write(stderr, "(3a, i0)") 'ASYNC NOTIFY of ',  str_ptr, &
               ' received from backend PID ', fptr%be_pid
            call PQfreemem(notify)
            nnotifies = nnotifies + 1
            res = PQconsumeInput(conn)
            notify = PQnotifies(conn)
         end do
      
      end block
   end do

   write(stderr, *) "Done."

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