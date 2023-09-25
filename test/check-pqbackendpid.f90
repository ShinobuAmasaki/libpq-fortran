program main
   use :: iso_c_binding
   use :: libpq
   implicit none
   
   type(c_ptr) :: conn
   print '(a)', "=== BEGIN TEST: PQbackendPID ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")

   print *, "backend PID:", PQbackendPID(conn)

   call PQfinish(conn)

   print '(a)', "===== END TEST ====="
   
end program main
    