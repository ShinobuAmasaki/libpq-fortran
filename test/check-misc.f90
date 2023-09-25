program main
   use libpq


   print '(a)', "=== BEGIN TEST: PQlibVersion ==="

   print *, PQlibVersion()

   print '(a)', "=== END TEST ==="

end program main