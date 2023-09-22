module error_message_fields
   use, intrinsic :: iso_fortran_env
   implicit none
   private

   integer(int32), parameter, public :: PG_DIAG_SEVERITY = ichar('S')
   integer(int32), parameter, public :: PG_DIAG_SEVERITY_NONLOCALIZED = ichar('V')
   integer(int32), parameter, public :: PG_DIAG_SQLSTATE = ichar( 'C')
   integer(int32), parameter, public :: PG_DIAG_MESSAGE_PRIMARY = ichar('M')
   integer(int32), parameter, public :: PG_DIAG_MESSAGE_DETAIL = ichar('D')
   integer(int32), parameter, public :: PG_DIAG_MESSAGE_HINT = ichar('H')
   integer(int32), parameter, public :: PG_DIAG_STATEMENT_POSITION = ichar('P')
   integer(int32), parameter, public :: PG_DIAG_INTERNAL_POSITION = ichar('p')
   integer(int32), parameter, public :: PG_DIAG_INTERNAL_QUERY = ichar('q')
   integer(int32), parameter, public :: PG_DIAG_CONTEXT = ichar('W')
   integer(int32), parameter, public :: PG_DIAG_SCHEMA_NAME = ichar('s')
   integer(int32), parameter, public :: PG_DIAG_TABLE_NAME = ichar('t')
   integer(int32), parameter, public :: PG_DIAG_COLUMN_NAME = ichar('c')
   integer(int32), parameter, public :: PG_DIAG_DATATYPE_NAME = ichar('d')
   integer(int32), parameter, public :: PG_DIAG_CONSTRAINT_NAME = ichar('n')
   integer(int32), parameter, public :: PG_DIAG_SOURCE_FILE = ichar('F')
   integer(int32), parameter, public :: PG_DIAG_SOURCE_LINE = ichar('L')
   integer(int32), parameter, public :: PG_DIAG_SOURCE_FUNCTION = ichar('R')

end module error_message_fields
