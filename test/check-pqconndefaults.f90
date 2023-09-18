program main
   use t_PQconninfoOption
   use m_fe_connect, only: PQconndefaults

   ! PQconninfoOption派生型の配列の宣言
   ! この配列はallocatableかたtargetでなければならない。
   ! この配列は手続PQconndefaultにより動的に割り当てられ、
   ! PQconninfoOptionのインスタンスを格納するのに使用される。
   !
   ! Declaration of a dynamic array of PQconninfoOption derived types.
   ! This array must be allocatable and target, which implies that it can be
   ! dynamically allocated and used to store instances of PQconninfoOption,
   ! typically allocated by the PQconndefaults procedure.
   type(PQconninfoOption), allocatable, target :: options(:)
   
   

   integer :: i

   ! 
   call PQconndefaults(options)

   do i = 1, size(options)
         print *, options(i)%keyword
         print *, options(i)%envvar
         print *, options(i)%compiled
         print *, options(i)%val
         print *, options(i)%label
         print *, options(i)%dispchar
         print *, options(i)%dispsize
         print *, '==================='
   end do

end program main
    