program main
   use libpq
   implicit none


print '(a)', "=== BEGIN TEST: PQisthreadsafe ==="
!==Add a test below===================================================!

   print *, "PQisthreadsafe() = ", PQisthreadsafe()

   

!==Test should be written above this line=============================!
print '(a)', "===== END TEST ====="

end program main