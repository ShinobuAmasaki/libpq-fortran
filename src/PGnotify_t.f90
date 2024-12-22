module PGnotify_t
   use, intrinsic :: iso_c_binding
   implicit none
   private

   type, public, bind(c) :: pgNotify
      type(c_ptr) :: relname
      integer(c_int) :: be_pid
      type(c_ptr) :: extra
   end type pgNotify

end module PGnotify_t