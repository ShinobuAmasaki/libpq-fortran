program main
   use, intrinsic :: iso_c_binding
   use libpq


   type(c_ptr) :: conn

   integer, parameter :: npairs = 3
   character(256, kind=c_char) :: keywords(npairs), values(npairs)

   integer :: res
   logical :: isContinue = .true.

   print '(a)', "=== BEGIN TEST: nonblocking - PQConnectStartParams ==="

   keywords(1) = "host"
   values(1)   = "localhost"
   
   keywords(2) = "user"
   values(2) = "postgres"

   keywords(3) = "dbname"
   values(3)  = "postgres"

   conn = PQconnectStartParams(keywords, values, 0)
   
   if (c_associated(conn)) then
      ! Enter Nonblocking connection

      do while (isContinue)
      res = PQconnectPoll(conn)

         select case (res)
         case (PGRES_POLLING_READING)
            isContinue = .true.
            print *, "POLLING READING"
         case (PGRES_POLLING_WRITING)
            isContinue = .true.
            print *, "POLLING WRITING"
         case (PGRES_POLLING_FAILED)
            isContinue = .false.
            print *, PQerrorMessage(conn)
            print *, "POLLING FAILED"
            exit
         case (PGRES_POLLING_OK)
            isContinue = .false.
            print *, "POLLING OK"
            print *, "Connection Established."
            exit
         end select

         call sleep(1)

      end do
      print *, PQstatus(conn)
   end if
   
   call PQfinish(conn)
   print '(a)', "===== END TEST ====="

   stop

end program main