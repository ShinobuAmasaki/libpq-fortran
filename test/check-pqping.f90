program main
   use :: libpq
   use, intrinsic :: iso_c_binding

   integer :: i

   integer :: res   
   character(:, kind=c_char), allocatable :: conninfo
   
   print '(a)', "=== BEGIN TEST: PQping ==="

   conninfo = 'host=localhost' 
   ! For other parameters, libpq uses environmental variables.

   res = PQping(conninfo)

   select case (res)
   case (PQPING_OK)
      print *, "PQPING OK"

   case (PQPING_REJECT)
      print *, "PQPING REJECT"

   case (PQPING_NO_RESPONSE)
      print *, "PQPING NO RESPONSE"

   case (PQPING_NO_ATTEMPT)
      print *, "PQPING NO ATTEMPT"

   case default
      print *, "UNKNOWN ERROR"
   end select

   print '(a)', "=== END TEST: check-pqping ==="

end program main