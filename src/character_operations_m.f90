module character_operations_m
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
                                       allocatable, target :: c_words(:)
      integer :: i, siz

      siz = size(words, dim=1)

      allocate( c_words(siz+1) )

      do i = 1, siz
         c_words(i) = trim(adjustl(words(i)))//c_null_char
      end do

      ! 最後の要素はNull文字を代入する。
      c_words(siz+1) = c_null_char

   end subroutine cchar_array_from_strings


   subroutine cchar_array_from_strings_no_null (words, c_words, max_length)
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      
      character(*), intent(in) :: words(:)
      integer(int32), intent(in) :: max_length

      character(max_length+1, kind=c_char), intent(out), allocatable, target :: c_words(:)

      integer :: i, siz
      
      siz = size(words, dim=1)
      allocate(c_words(siz))

      do i = 1, siz
         c_words(i) = trim(adjustl(words(i)))//c_null_char
      end do
   end subroutine cchar_array_from_strings_no_null


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
  

      ! Input: c_words
   ! Output: ptr_array (with null pointer termination)
   subroutine cptr_array_from_cchar_no_null (c_words, ptr_array)
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

   end subroutine cptr_array_from_cchar_no_null
   
   subroutine read_option(sizes, c_option, option)
      use :: PQconninfoOption_t
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_PQconnoptionSizes), intent(in) :: sizes
      type(c_PQconninfoOption), intent(inout) :: c_option
      type(PQconninfoOption), intent(out) :: option

      ! Cの構造体からFortranの派生型に、keywordの値をコピーする。
      block
         character(sizes%keyword), pointer :: keyword

         call c_f_pointer(c_option%keyword, keyword)
         option%keyword = trim(keyword)
      end block

      if (sizes%envvar > 0) then
         block
            character(sizes%envvar), pointer :: envvar
            call c_f_pointer(c_option%envvar, envvar)
            option%envvar = trim(envvar)
         end block
      else 
         option%envvar = '' 
      end if

      if (sizes%compiled >0) then
         block
            character(sizes%compiled), pointer :: compiled
            call c_f_pointer(c_option%compiled, compiled)
            option%compiled = trim(compiled)
         end block 
      else
         option%compiled = ''
      end if

      if (sizes%val >0) then
         block
            character(sizes%val), pointer :: val
            call c_f_pointer(c_option%val, val)
            option%val = trim(val)
         end block
      else
         option%val = ''
      end if

      if (sizes%label > 0) then
         block
            character(sizes%label), pointer :: label
            call c_f_pointer(c_option%label, label)
            option%label = trim(label)
         end block
      else
         option%label = ''
      end if

      if (sizes%dispchar > 0) then
         block
            character(1), pointer :: dispchar
            call c_f_pointer(c_option%dispchar, dispchar)
            option%dispchar = trim(dispchar)
         end block 
      else
         option%dispchar = ''
      end if

      block
         option%dispsize = c_option%dispsize
      end block

   end subroutine read_option


   subroutine c_char_to_f_string (cptr, str)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_size_t
      use, intrinsic :: iso_fortran_env
      implicit none
      type(c_ptr), intent(in) :: cptr
      character(:), allocatable, intent(out) :: str

      character(:, kind=c_char), allocatable :: buf

      integer(c_size_t) :: length

      interface 
         ! Interface to 'strlen' C function
         function strlen(ptr) bind(c)
            import c_ptr, c_size_t
            type(c_ptr), intent(in), value :: ptr
            integer(c_size_t) :: strlen
         end function strlen
      end interface

      length = strlen(cptr)

      call  convert_cptr(cptr, length, buf)
      str = buf

   contains
      ! `convert_cptr` takes a string with type(cptr) and its length as input,
      ! and returns a corresponding Fortran pointer of character-type.
      subroutine convert_cptr(cptr, length, buf)
         use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t, c_f_pointer, c_char
         implicit none
         type(c_ptr), intent(in) :: cptr
         integer(c_size_t), intent(in) :: length
         character(:, kind=c_char), allocatable, intent(out)  :: buf
         
         ! Declare a character-type pointer variable `fptr` with a length `length` and
         ! a kind parameter `c_char`.
         character(len=length, kind=c_char), pointer :: fptr

         call c_f_pointer(cptr, fptr)

         buf = fptr

      end subroutine convert_cptr

   end subroutine c_char_to_f_string


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

   end function c_to_f_charpointer
      
   function c_to_f_charpointer_with_length(char_cptr, length) result(res)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_char
      use, intrinsic :: iso_fortran_env
      implicit none
      type(c_ptr), intent(in) :: char_cptr

      integer(int32) :: length
      character(length, kind=c_char), pointer :: res

      res => convert_cptr(char_cptr, int(length, int64))
   end function 

         ! `convert_cptr` takes a string with type(cptr) and its length as input,
      ! and returns a corresponding Fortran pointer of character-type.
   function convert_cptr(cptr, length) result(fptr)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t, c_f_pointer, c_char
      implicit none
      type(c_ptr), intent(in) :: cptr
      integer(c_size_t), intent(in) :: length

      ! Declare a character-type pointer variable `fptr` with a length `length` and
      ! a kind parameter `c_char`.
      character(len=length, kind=c_char), pointer :: fptr

      call c_f_pointer(cptr, fptr)

   end function convert_cptr


end module character_operations_m