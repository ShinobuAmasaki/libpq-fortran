program main
   use libpq
   use iso_c_binding
   implicit none

   type(c_ptr) :: res
   character(:), allocatable :: str

   print '(a)', "=== BEGIN TEST: PQsslAttribute-noconnections  ==="
!==Add a test below===================================================!

   call  PQsslAttribute(c_null_ptr, 'library', str)

   print *, str

!==Test should be written above this line=============================!
print '(a)', "===== END TEST ====="

end program main
