program main
   use, intrinsic :: iso_c_binding
   use libpq


   type(c_ptr) :: conn

   character(256, kind=c_char) :: conninfo

   integer :: res
   logical :: isContinue = .true.

   print '(a)', "=== BEGIN TEST: nonblocking ==="

   conninfo = "host=localhost user=postgres dbname=postgres"

   conn = PQconnectStart(conninfo)
   
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