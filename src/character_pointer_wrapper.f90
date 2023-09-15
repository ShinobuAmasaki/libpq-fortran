module character_pointer_wrapper
   implicit none
   private
   public :: c_to_f_charpointer

contains

   ! `c_to_f_charpointer` function takes a pointer to a string with type(c_ptr) as input,
   ! and returns a corresponding Fortran pointer of character-type.
   function c_to_f_charpointer(char_cptr) result(res)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_size_t
      implicit none
      type(c_ptr), intent(in), value :: char_cptr

      character(:, kind=c_char), pointer :: res

      interface 
         ! Interface to 'strlen' C function
         function strlen(ptr) bind(c)
            import c_ptr, c_size_t
            type(c_ptr), value :: ptr
            integer(c_size_t)  :: strlen
         end function strlen
      end interface

      ! Associate the pointer variable `res` with the result of the function `convert_cptr`.
      res => convert_cptr(char_cptr, strlen(char_cptr))

   contains
      
      ! `convert_cptr` takes a string with type(cptr) and its length as input,
      ! and returns a corresponding Fortran pointer of character-type.
      function convert_cptr(cptr, length) result(fptr)
         use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t, c_f_pointer
         implicit none
         type(c_ptr), intent(in) :: cptr
         integer(c_size_t), intent(in) :: length

         ! Declare a character-type pointer variable `fptr` with a length `length` and
         ! a kind parameter `c_char`.
         character(len=length, kind=c_char), pointer :: fptr

         call c_f_pointer(cptr, fptr)

      end function convert_cptr

   end function c_to_f_charpointer

end module character_pointer_wrapper