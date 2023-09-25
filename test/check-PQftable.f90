program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: sql
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQftable ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   sql = 'select datname from pg_database;'

   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      print *, PQresultStatus(res)
      print *, PQresulterrorMessage(res)
   end if

   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   do i = 0, PQnfields(res)-1
      print *, 'Oid of ', PQfname(res, i),' is', PQftable(res, i)
   end do


!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main