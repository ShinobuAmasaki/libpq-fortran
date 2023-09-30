program main
   use :: libpq
   use :: iso_c_binding

   type(c_ptr) :: conn, res

   character(:, kind=c_char), allocatable :: command

   integer :: ires

   print *, "=== BEGIN TEST: PQsendQueryParams"

   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
   if (PQstatus(conn) /= CONNECTION_OK) then
      print *, PQerrorMessage(conn)
      call PQfinish(conn)
      error stop
   end if

   command = "select $1::bigint + $2::bigint;"

   ires = PQsendQueryParams(conn, command, 2, [0,0], ["500", "300"])

   if (ires /= 1) then
      print *, PQerrorMessage(conn)
      call PQfinish(conn)
      error stop
   end if

   res = PQgetResult(conn)

   do while (c_associated(res))

      if (PQresultStatus(res) /= PGRES_TUPLES_OK) print *, PQerrorMessage(conn)

      print *, PQgetvalue(res, 0, 0)   ! the result "800" is expected. 
      call PQclear(res)

      res = PQgetResult(conn)

   end do

   call PQclear(res)
   call PQfinish(conn)

   print *, "====== END TEST ======"

end program main



