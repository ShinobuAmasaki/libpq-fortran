program main
   use :: libpq
   use, intrinsic :: iso_c_binding

   integer :: i

   integer :: res

   character(256, kind=c_char) :: keywords(2), values(2)
   print '(a)', "=== BEGIN TEST: check-pqpingparams ==="
   keywords(1) = "hostaddr"
   values(1)   = "127.0.0.1"

   keywords(2) = "user"
   values(2)   = "postgres"
   

   res = PQpingParams(keywords, values, 0)

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

   print '(a)', "=== END TEST: check-pqpingparams ==="

end program main