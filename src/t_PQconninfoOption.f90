module t_PQconninfoOption
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


   type, public :: PQconninfoOption
      character(:), allocatable :: keyword
      character(:), allocatable :: envvar
      character(:), allocatable :: compiled
      character(:), allocatable :: val
      character(:), allocatable :: label
      character(:), allocatable :: dispchar
      integer(int32)            :: dispsize
   end type

   type, public :: c_PQconninfoOption
      type(c_ptr) :: keyword
      type(c_ptr) :: envvar
      type(c_ptr) :: compiled
      type(c_ptr) :: val
      type(c_ptr) :: label
      type(c_ptr) :: dispchar
      integer(c_int) :: dispsize
   end type

end module t_PQconninfoOption