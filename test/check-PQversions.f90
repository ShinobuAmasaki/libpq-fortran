program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn

   character(:, kind=c_char), allocatable :: conninfo, sql

   integer :: major, minor

   print '(a)', "=== BEGIN TEST: pqconnectdb ==="
   
   conninfo = "host=localhost user=postgres dbname=postgres"

   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if

   major = PQserverVersion(conn)/10000
   minor = mod(PQserverVersion(conn),10000)

   print '(1x, a, i0)', 'protocol version : ', PQprotocolVersion(conn)
   print '(1x, a, i0,a,i0)', 'server version   : ', major,'.',minor
   
   call PQfinish(conn)


   print '(a)', "===== END TEST ====="
end program main