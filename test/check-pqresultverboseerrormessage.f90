program main
   use libpq
   use iso_c_binding

   type(c_ptr) :: conn, res

   character(:, kind=c_char), allocatable :: sql

   print '(a)', "=== BEGIN TEST:  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
!==Add a test below===================================================!

   sql = '? invalid sql;'

   res = PQexec(conn, sql)

   if (PQresultStatus(res) /= 0) then
      print *, '= Normal Error Message ='
      print *, PQresulterrorMessage(res)

      print *, '= PQERRORS_VERBOSE ='
      print *, PQresultVerboseErrorMessage(res, PQERRORS_VERBOSE, PQSHOW_CONTEXT_ERRORS)
   end if
   

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main