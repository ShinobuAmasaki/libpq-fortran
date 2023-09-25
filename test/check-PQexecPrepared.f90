program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: query, name
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQexecPrepared ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   name = "eight-zero-zero"

!-- Prepare statement
   query = "select $1::bigint + $2::bigint;"

   res = PQprepare(conn, name, query, 2, [0, 0])
   if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      print *, PQerrorMessage(conn)
   end if 


!-- Execute prepared statement
   res = PQexecPrepared(conn, name, 2, ["300", "500"])
   
   if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      print *, PQerrorMessage(conn)
   end if 
   
!-- Display the result
   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)   ! will display 800.  
   end do

!==Test should be written above this line=============================!
   call PQclear(res)
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main