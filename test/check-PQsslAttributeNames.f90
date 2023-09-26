program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: conn, res
   character(:), allocatable :: sql
   character(11), allocatable :: strings(:)
   integer :: i, siz, maxlen

   print '(a)', "=== BEGIN TEST: PQsslAttributeNames ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   call PQsslAttributeNames(conn, strings, 11)

   do i = 1, size(strings, dim=1)
      print *, strings(i)
   end do


!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="



end program main