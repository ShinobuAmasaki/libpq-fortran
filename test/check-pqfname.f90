program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn, res
   integer :: i

   character(:, kind=c_char), allocatable :: conninfo, sql

   print '(a)', "=== BEGIN TEST: PQfname ==="

   conninfo = "host=localhost user=postgres"

   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if

   sql = 'select * from pg_database;'

   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0) then
      print *, PQresultStatus(res)
      print *, PQresulterrorMessage(res)
   end if

   print *, 'query: "'//trim(sql)//'"'
   do i = 0, PQnfields(res)-1
      print *, PQfname(res, i)
   end do

   call PQclear(res)
   call PQfinish(conn)

   print '(a)', "=== END TEST: check-pqfname ==="

end program main