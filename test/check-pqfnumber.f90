program main
   use libpq
   use iso_c_binding
   implicit none
   

   type(c_ptr) :: conn,res

   print '(a)', "=== BEGIN TEST:  ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
!==Add a test below===================================================!

   res = PQexec(conn, 'select 1 as FOO, 2 as "BAR";')

   print *, 'PQfname(res, 0):', PQfname(res, 0)
   print *, 'PQfname(res, 1):', PQfname(res, 1)
   print *, 'PQfnumber(res, "FOO")  :', PQfnumber(res, "FOO")
   print *, 'PQfnumber(res, "foo")  :', PQfnumber(res, "foo")
   print *, 'PQfnumber(res, "BAR")  :', PQfnumber(res, "BAR")
   
   print *, "PQfnumber(res, '"//'"BAR"'//"'):", PQfnumber(res, '"BAR"')
   
   ! In Fortran, both single quotes and double quates can be used in a similar way,
   ! so if you want to include double quotes in a string, 
   ! you can simply use single quotes, and unlike C language, there is no need to escape them.


!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main