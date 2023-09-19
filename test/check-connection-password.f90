program main
   use libpq
   use iso_c_binding

   type(c_ptr) :: conn

   print '(a)', "=== BEGIN TEST:  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
!==Add a test below===================================================!
   block
      integer :: res

      res = PQconnectionNeedsPassword(conn)
      print *, "Connection needs password? > ", res

      res = PQconnectionUsedPassword(conn)
      print *, "Connection used password? > ", res

   end block
   

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main