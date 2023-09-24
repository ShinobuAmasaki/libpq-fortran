program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: query, name
   integer :: i 

   print '(a)', "=== BEGIN TEST: PQdescribePrepared ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   name = "eight-zero-zero"

!-- Prepare statement
   query = "select $1::integer as foo;"    ! single INTEGER parameter

   res = PQprepare(conn, name, query, 1, [0])

      ! When PQprepare executed successfully, the PQresultStatus returns PGRES_COMMAND_OK
   if (PQresultStatus(res) /= PGRES_COMMAND_OK) then
      print *, PQerrorMessage(conn)
   end if 


!-- Describe prepared statement
   res = PQdescribePrepared(conn, name)
   
   ! When PQdescribedPrepared executed successfully, the PQresultStatus returns PGRES_COMMAND_OK
   if (PQresultStatus(res) /= PGRES_COMMAND_OK) then
      print *, PQerrorMessage(conn)
   end if

   print *, PQnparams(res)       ! will displays 1
   print *, PQparamtype(res, 0)  ! will displays oid = 23 (that means integer)

!-- Execute prepared statement
   res = PQexecPrepared(conn, name, 1, ["900"])
   if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
      print *, PQerrorMessage(conn)
   end if 

   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)
   end do

!-- Display the result


!==Test should be written above this line=============================!
   call PQclear(res)
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main