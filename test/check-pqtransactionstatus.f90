program main
   use :: libpq
   use, intrinsic :: iso_c_binding
   implicit none
   
   type(c_ptr) :: conn

   integer, parameter :: n = 1

   integer :: res

   character(256, kind=c_char) :: keywords(n), values(n)

   print '(a)', "=== BEGIN TEST: check-pqconnectdbparams ==="

   keywords(1) = 'hostaddr'
   values(1) = '127.0.0.1'


   conn = PQconnectdbParams(keywords, values, 0)
   
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
   end if

   res = PQtransactionStatus(conn)

   select case (res)
   case (PQTRANS_IDLE)
      print *, "PQTRANS_IDLE"
   case (PQTRANS_ACTIVE)
      print *, "PQTRANS_ACTIVE"
   case (PQTRANS_INTRANS)
      print *, "PQTRANS_INTRANS"
   case (PQTRANS_INERROR)
      print *, "PQTRANS_INERROR"
   case (PQTRANS_UNKNOWN)
      print *, "PQTRANS_UNKNOWN"
   case default
      print *, "ERROR"
   end select

   call PQfinish(conn)
   print '(a)', "===== END TEST ====="

end program main