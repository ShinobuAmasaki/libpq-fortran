module character_operations
   implicit none
   
contains

   function max_length_char_array (array) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: array(:)
      integer(int32) :: res, i

      res = 0
      do i = lbound(array, dim=1), ubound(array, dim=1)
         res = max(res, len_trim(array(i)))
      end do

   end function max_length_char_array


   subroutine cchar_array_from_strings (words, c_words, max_length)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      character(*), intent(in)   :: words(:)
      integer(int32), intent(in) :: max_length

      character(max_length+1, kind=c_char), intent(out), &
                                       allocatable :: c_words(:)
      integer :: i, siz

      siz = size(words, dim=1)

      allocate( c_words(siz) )

      do i = 1, siz
         c_words(i) = trim(adjustl(words(i)))//c_null_char
      end do

   end subroutine cchar_array_from_strings


   ! Input: c_words
   ! Output: ptr_array (with null pointer termination)
   subroutine cptr_array_from_cchar (c_words, ptr_array)
      use, intrinsic :: iso_c_binding
      implicit none
      
      character(*, kind=c_char), intent(in), target :: c_words(:)
      type(c_ptr), allocatable, intent(out) :: ptr_array(:)

      integer :: siz, i
      siz = size(c_words, dim=1)

      allocate(ptr_array(siz+1))

      do i = 1, siz
         ptr_array(i) = c_loc(c_words(i))
      end do

      ptr_array(siz) = c_null_ptr

   end subroutine cptr_array_from_cchar
   

end module character_operations