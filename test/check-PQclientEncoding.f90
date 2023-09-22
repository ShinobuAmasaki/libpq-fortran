program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQclientEncoding  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   print *, PQclientEncoding(conn)
   

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main