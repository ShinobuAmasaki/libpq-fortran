program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: sql
   integer :: i, stat

   print '(a)', "=== BEGIN TEST: PQresStatus  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   sql = 'select * from pg_database;'

   print *, "PQexec: ", trim(sql)

   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      print *, PQresultStatus(res)
      print *, PQresulterrorMessage(res)
   end if

   stat = PQresultStatus(res)
   print *,"PQresStatus: ", PQresStatus(stat)   

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main