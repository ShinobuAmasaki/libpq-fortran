program main
   use :: libpq
   use, intrinsic :: iso_c_binding

   integer :: i

   type(c_ptr) :: conn, res

   character(:, kind=c_char), allocatable :: sql, conninfo

   print '(a)', "=== BEGIN TEST: check-pqexec ==="

   !conninfo = '' ! If this is empty, libpq uses only environmental variables for a connection. 
   
   conninfo = 'host=localhost user=postgres'
   
   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if

   sql = 'select datname from pg_database;'

   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      print *, PQresultStatus(res)
      print *, PQresulterrorMessage(res)
   end if
! 
   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)
   end do

   call PQclear(res)
   call PQfinish(conn)

   print '(a)', "=== END TEST: check-pqexec ==="

end program main