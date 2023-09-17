program main
   use :: libpq
   use, intrinsic :: iso_c_binding
   implicit none
   

   type(c_ptr) :: conn

   print '(a)', "=== BEGIN TEST: check-pqreset ==="
   
   conn = PQsetdbLogin("localhost", "5432", "", "", "postgres","postgres", "")

   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
   end if
   print *, 'database : ', PQdb(conn)
   print *, 'username : ', PQuser(conn)
   print *, 'host     : ', PQhost(conn)
   print *, 'hostaddr : ', PQhostaddr(conn)
   
   print *, "  == PQreset =="
   call PQreset(conn)

   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
   end if



   print *, 'database : ', PQdb(conn)
   print *, 'username : ', PQuser(conn)
   print *, 'host     : ', PQhost(conn)
   print *, 'hostaddr : ', PQhostaddr(conn)

   call PQfinish(conn)
   print '(a)', "===== END TEST ====="

end program main