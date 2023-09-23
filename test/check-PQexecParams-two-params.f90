program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: sql
   character(256) :: values(2)
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQexecParams ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   print *, 'test:'

   sql = "select $1::bigint + $2::bigint;"

   values(1) = "300"
   values(2) = "500"

   res = PQexecParams(conn, sql, 2, [0, 0], values)

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