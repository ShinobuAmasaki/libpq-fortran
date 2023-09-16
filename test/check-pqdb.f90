program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn

   character(:, kind=c_char), allocatable :: conninfo, sql

   print '(a)', "=== BEGIN TEST: check-pqdb,      ==="
   print '(a)', "                      pquser,"
   print '(a)', "                      pqhost,"
   print '(a)', "                      pqhostaddr"

   conninfo = "host=localhost"

   conn = PQconnectdb(conninfo)
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