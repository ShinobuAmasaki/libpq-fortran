program main
   use :: libpq
   use, intrinsic :: iso_c_binding
   implicit none
   

   type(c_ptr) :: conn, res

   character(:, kind=c_char), allocatable :: command

   integer :: ires
   logical :: flag 

   print '(a)', "=== BEGIN TEST: PQisnonblocking ==="

   conn = PQconnectdb("user=postgres dbname=postgres")


   ires = PQsetnonblocking(conn, 1) ! set nonblocking
   ! ires = PQsetnonblocking(conn, 0) ! set blocking
   

   flag = PQisnonblocking(conn)

   print *, flag

   print '(a)', "=== END TEST ==="

   call PQfinish(conn)

end program main