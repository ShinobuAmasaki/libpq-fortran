program main
   use libpq
   use iso_c_binding
   implicit none
   

   character(:), allocatable :: conninfo
   character(256) :: errmsg
   logical :: errflag 
   integer :: i
   type(PQconninfoOption), allocatable, target :: options(:)

   print '(a)', "=== BEGIN TEST: PQconninfoParse ==="
!==Add a test below===================================================!
   conninfo = "host=localhost user=postgres dbname=postgres"

   call PQconninfoParse(conninfo, options, errmsg, errflag)

   if (.not. errflag) then
      do i = 1, size(options)
         print '(12a, i0)', trim(options(i)%keyword)," : ", &
            trim(options(i)%envvar), " : ", &
            trim(options(i)%compiled), " : ", &
            trim(options(i)%val), " : ", &
            trim(options(i)%label), " : ", &
            trim(options(i)%dispchar), " : ", &
            options(i)%dispsize
      end do
   else
      print *, trim(errmsg)
   end if

   deallocate(options)
!==Test should be written above this line=============================!
print '(a)', "===== END TEST ====="

end program main