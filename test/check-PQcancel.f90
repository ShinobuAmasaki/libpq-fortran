program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(256) :: errbuf
   integer :: i 

   print '(a)', "=== BEGIN TEST:  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   errbuf = ''

   res = PQgetCancel(conn)

   print *, PQcancel(res, errbuf, 256)

   print *, trim(errbuf)

   call PQfreeCancel(res)
   

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main