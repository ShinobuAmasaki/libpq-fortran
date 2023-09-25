program main
   use :: libpq
   use, intrinsic :: iso_c_binding
   implicit none
   
   type(c_ptr) :: conn

   integer, parameter :: npairs = 3
   character(256,kind=c_char) :: keywords(npairs), values(npairs)

   print '(a)', "=== BEGIN TEST: PQoptions ==="

   keywords(1) = "host"
   values(1) = "localhost"

   keywords(2) = "dbname"
   values(2) = "sandbox"

   keywords(3) = "options"
   values(3) = "-c geqo=off"

   conn = PQconnectdbParams(keywords, values, 0)

   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
   end if

   print *, 'options: "', PQoptions(conn), '"'

   call PQfinish(conn)
   print '(a)', "===== END TEST ====="

end program main