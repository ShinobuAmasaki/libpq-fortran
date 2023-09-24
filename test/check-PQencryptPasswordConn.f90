program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn
   character(:), allocatable :: sql
   character(:), pointer :: res
   character(256) :: encrypted
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQencryptPasswordConn  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   call PQencryptPasswordConn(conn, "foobarbaz", "postgres", "scram-sha-256", encrypted)

   ! For scram-sha-256, it is normal for the result to vary for each connection session.
   print *, trim(encrypted)

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main