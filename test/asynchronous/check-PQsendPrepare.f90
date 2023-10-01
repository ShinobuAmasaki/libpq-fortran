program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn, res

   character(:, kind=c_char), allocatable :: conninfo, query

   integer :: ires

   print '(a)', "=== BEGIN TEST: PQsendPrepare       ==="
   print '(a)', '                PQsendQueryPrepared '

   conninfo = "host=localhost user=postgres dbname=postgres"

   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      call PQfinish(conn)
      error stop
   end if

   query = "select $1::bigint + $2::bigint;"

   ires = PQsetnonblocking(conn, 1)

   ires = PQsendPrepare(conn, "add", query, 2, [0, 0])
   if (ires /= 1) then
      print *, PQerrorMessage(conn)
   end if

   res = PQgetResult(conn)

   do while (c_associated(res))

      if (PQresultStatus(res) /= PGRES_COMMAND_OK) then
         print *, PQerrorMessage(conn)
      end if
      call PQclear(res)
      res = PQgetResult(conn)

   end do

   ires = PQsendQueryPrepared(conn, "add", 2, ["500", "300"])
   if (ires /= 1) then
      print *, PQerrorMessage(conn)
   end if

   res = PQgetResult(conn)

   do while (c_associated(res))

      if (PQresultStatus(res) /= PGRES_TUPLES_OK) then
         print *, PQerrorMessage(conn)
      end if 

      print *, PQgetvalue(res, 0, 0)   ! The result "800" is expected.
      call PQclear(res)
   
      res = PQgetResult(conn)

   end do

      
   call PQclear(res)
   call PQfinish(conn)


   print '(a)', "===== END TEST ====="
end program main