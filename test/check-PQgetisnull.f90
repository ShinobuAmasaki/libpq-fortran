program main
   use libpq
   use iso_c_binding

   type(c_ptr) :: conn, res

   character(:), allocatable :: sql

   print '(a)', "=== BEGIN TEST: PQgetisnull  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   sql = "select 1 as foo;"

   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      print *, PQresultStatus(res)
      print *, PQresulterrorMessage(res)
   end if

   print *, "query: ", sql
   print *, "PQgetisnull (0,0): ", PQgetisnull(res, 0, 0)

   sql = "select null as foo;"
   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      print *, PQresultStatus(res)
      print *, PQresulterrorMessage(res)
   end if
   print *, "query: ", sql
   print *, "PQgetisnull (0,0): ", PQgetisnull(res, 0, 0)


!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main