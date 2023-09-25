program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn

   print '(a)', "=== BEGIN TEST: check-pqsetdblogin ==="
   conn = PQsetdbLogin("localhost", "5432", "", "", "postgres","postgres", "")

   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if

   print *, 'database : ', PQdb(conn)
   print *, 'username : ', PQuser(conn)
   print *, 'host     : ', PQhost(conn)
   print *, 'hostaddr : ', PQhostaddr(conn)

   call PQfinish(conn)


   print '(a)', "===== END TEST ====="
end program main