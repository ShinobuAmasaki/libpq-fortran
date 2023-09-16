program main
   use :: libpq
   use, intrinsic :: iso_c_binding
   implicit none
   

   type(c_ptr) :: conn

   integer, parameter :: npairs = 1
   character(256, kind=c_char) :: keywords(npairs), values(npairs)


   print '(a)', "=== BEGIN TEST: check-pqconnectdbparams ==="

   keywords(1) = "hostaddr"
   values(1)   = "127.0.0.1"

   
   conn = PQconnectdbParams(keywords, values, 0)

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