program main
   use libpq
   use iso_c_binding
   use iso_fortran_env
   implicit none

   type(c_ptr) :: conn
   character(:), allocatable :: sql
   integer :: i 


   character(256) :: errmsg = ''
   character(:), pointer :: res

   print '(a)', "=== BEGIN TEST:  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   sql = "'hogeFUGA'"

   res => PQescapeIdentifier(conn, sql, len_trim(sql, int64))

   print *, res
   
!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main