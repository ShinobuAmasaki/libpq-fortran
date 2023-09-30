program main
   use PQconninfoOption_t
   use libpq, only: PQconndefaults

   ! PQconninfoOption派生型の配列の宣言
   ! この配列はallocatableかつtargetでなければならない。
   ! この配列は手続PQconndefaultにより動的に割り当てられ、
   ! PQconninfoOptionのインスタンスを格納するのに使用される。
   !
   ! Declaration of a dynamic array of PQconninfoOption derived types.
   ! This array must be allocatable and target, which implies that it can be
   ! dynamically allocated and used to store instances of PQconninfoOption,
   ! typically allocated by the PQconndefaults procedure.
   type(PQconninfoOption), allocatable, target :: options(:)
   

   integer :: i
   print '(a)', "=== BEGIN TEST: PQconndefaults ==="
   ! 
   call PQconndefaults(options)

   do i = 1, size(options)
      print '(12a, i0)', trim(options(i)%keyword)," : ", &
         trim(options(i)%envvar), " : ", &
         trim(options(i)%compiled), " : ", &
         trim(options(i)%val), " : ", &
         trim(options(i)%label), " : ", &
         trim(options(i)%dispchar), " : ", &
         options(i)%dispsize
   end do

   print '(a)', "===== END TEST ====="
end program main
    