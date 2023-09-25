program main
   use libpq
   use iso_c_binding

   type(c_ptr) :: conn
   type(PQconninfoOption), allocatable, target :: options(:)
   integer :: i

   print '(a)', "=== BEGIN TEST: PQconninfo ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
!==Add a test below===================================================!

   call PQconninfo(conn, options)

   do i = 1, size(options)
      print '(12a, i0)', trim(options(i)%keyword)," : ", &
         trim(options(i)%envvar), " : ", &
         trim(options(i)%compiled), " : ", &
         trim(options(i)%val), " : ", &
         trim(options(i)%label), " : ", &
         trim(options(i)%dispchar), " : ", &
         options(i)%dispsize
   end do

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main