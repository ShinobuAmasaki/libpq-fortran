program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: sql
   integer :: i 

   print '(a)', "=== BEGIN TEST:  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   sql = "select 1;"

   res = PQexec(conn, sql)

   print *, PQcmdStatus(res)  ! 'SELECT 1' is expected.
   print *, PQcmdTuples(res)  ! 1 is expected
   print *, PQoidValue(res)   ! 0 is expected

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main