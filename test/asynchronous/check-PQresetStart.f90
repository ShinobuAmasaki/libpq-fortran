program main
   use, intrinsic :: iso_c_binding
   use libpq


   type(c_ptr) :: conn

   character(256, kind=c_char) :: conninfo

   integer :: res
   logical :: isContinue = .true.

   print '(a)', "=== BEGIN TEST: nonblocking-PQresetStart ==="

   conninfo = "host=localhost user=postgres dbname=postgres"

   conn = PQconnectStart(conninfo)
   
   if (c_associated(conn)) then
      do while (isContinue)
         res = PQconnectPoll(conn)
         select case (res)
         case (PGRES_POLLING_READING)
            isContinue = .true.
         case (PGRES_POLLING_WRITING)
            isContinue = .true.
         case (PGRES_POLLING_FAILED)
            isContinue = .false.
            print *, PQerrorMessage(conn)
            print *, "POLLING FAILED"
            exit
         case (PGRES_POLLING_OK)
            isContinue = .false.
            print *, "Connection established."
            exit
         end select
         call sleep(1)
      end do
   end if

   print *, "Try to reset the connection."
   res = PQresetStart(conn)
   if (res == 0) then
      print *, "PQresetStart is failed."
   else
      isContinue = .true.

      do while (isContinue)
      
         res = PQresetPoll(conn)
         select case (res)
         case (PGRES_POLLING_READING)
            isContinue = .true.
         case (PGRES_POLLING_WRITING)
            isContinue = .true.
         case (PGRES_POLLING_FAILED)
            isContinue = .false.
            print *, PQerrorMessage(conn)
            print *, "POLLING FAILED"
            exit
         case (PGRES_POLLING_OK)
            isContinue = .false.
            print *, "Reset connection established."
            exit
         end select
         call sleep(1)
      
      end do
      
   end if

   call PQfinish(conn)

   print '(a)', "===== END TEST ====="

end program main

   