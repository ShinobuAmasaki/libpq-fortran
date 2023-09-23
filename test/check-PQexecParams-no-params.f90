program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: sql
   character(256) :: values(1)
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQexecParams no paramters ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   print *, "test: PQexecParams(conn, sql, 0, [0_8], [''], [0], [0])"

   sql = "select 100;"

   res = PQexecParams(conn, sql, 0, [0_8], [""])

   if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      print *, PQerrorMessage(conn)
      call PQclear(res)
      call PQfinish(conn)
      stop 
   end if 

   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)
   end do

!==Test should be written above this line=============================!
   
   call PQclear(res)
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main