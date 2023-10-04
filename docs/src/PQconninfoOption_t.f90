module PQconninfoOption_t
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   implicit none


! Quote from 218-240 lines of postgresql-15.4/src/libpq-fe.h

!/* ----------------
! * Structure for the conninfo parameter definitions returned by PQconndefaults
! * or PQconninfoParse.
! *
! * All fields except "val" point at static strings which must not be altered.
! * "val" is either NULL or a malloc'd current-value string.  PQconninfoFree()
! * will release both the val strings and the PQconninfoOption array itself.
! * ----------------
! */
!   typedef struct _PQconninfoOption
!   {
!      char	   *keyword;		/* The keyword of the option			*/
!      char	   *envvar;			/* Fallback environment variable name	*/
!      char	   *compiled;		/* Fallback compiled in default value	*/
!      char	   *val;			/* Option's current value, or NULL		 */
!      char	   *label;			/* Label for field in connect dialog	*/
!      char	   *dispchar;		/* Indicates how to display this field in a
!                            * connect dialog. Values are: "" Display
!                            * entered value as is "*" Password field -
!                            * hide value "D"  Debug option - don't show
!                            * by default */
!      int			dispsize;		/* Field size in characters for dialog	*/
!   } PQconninfoOption;

! END Quote
   
   integer, parameter :: CONNINFO_LABEL_LEN = 64 

   !> This is a derived-type that you are expected to use. 
   type, public :: PQconninfoOption
      !> The keyword of the option.
      character(CONNINFO_LABEL_LEN) :: keyword

      !> Fallback environment variable name
      character(CONNINFO_LABEL_LEN) :: envvar

      !> Fallback compiled in default value
      character(CONNINFO_LABEL_LEN) :: compiled

      !> Option's current value, or empty.
      character(CONNINFO_LABEL_LEN) :: val

      !> Lable for field in connect dialog
      character(CONNINFO_LABEL_LEN) :: label

      !| Indicates how to display this field in a connect dialog.
      !  Values are: <br>
      ! `""`  &#009; Display entered value as is,<br>
      !  `"*"` &#009; Password field - hide value,<br>
      !  `"D"` &#009; Debug option - don't show by default.
      character(1) :: dispchar

      !> Field size in characters for dialog.
      integer(int32)            :: dispsize
   end type

   !> This type is for wraps, such as `[[PQconninfo]]`, `[[PQconndefaults]]`, `[[PQconninfoParse]]`. 
   type, public :: c_PQconninfoOption
      type(c_ptr) :: keyword
      type(c_ptr) :: envvar
      type(c_ptr) :: compiled
      type(c_ptr) :: val
      type(c_ptr) :: label
      type(c_ptr) :: dispchar
      integer(c_int) :: dispsize
   end type

   !> This type is for wraps, such as `[[PQconninfo]]`, `[[PQconndefaults]]`, `[[PQconninfoParse]]`. 
   type, public :: c_PQconnOptionSizes
      integer(c_int) :: keyword
      integer(c_int) :: envvar
      integer(c_int) :: compiled
      integer(c_int) :: val
      integer(c_int) :: label
      integer(c_int) :: dispchar
   end type




end module PQconninfoOption_t