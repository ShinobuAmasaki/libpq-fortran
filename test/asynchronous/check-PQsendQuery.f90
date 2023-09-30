program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn, res

   character(:, kind=c_char), allocatable :: conninfo, command

   integer :: ires

   print '(a)', "=== BEGIN TEST: PQsendQuery ==="
   
   conninfo = "host=localhost user=postgres dbname=postgres"

   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      call PQfinish(conn)
      error stop
   end if

   command = "select 1234;"

   ires = PQsendQuery(conn, command)
   if (ires /= 1) then
      print *, PQerrorMessage(conn)
   end if

   res = PQgetResult(conn)

   do while (c_associated(res))

      if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
         print *, PQerrorMessage(conn)
      end if

      print *, PQgetvalue(res, 0, 0)
      call PQclear(res)
   
      res = PQgetResult(conn)

   end do
      
   call PQclear(res)
   call PQfinish(conn)


   print '(a)', "===== END TEST ====="
end program main