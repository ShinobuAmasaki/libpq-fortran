module error_message_fields
   implicit none
   private

   character(1), parameter, public :: PG_DIAG_SEVERITY = 'S'
   character(1), parameter, public :: PG_DIAG_SEVERITY_NONLOCALIZED = 'V'
   character(1), parameter, public :: PG_DIAG_SQLSTATE = 'C'
   character(1), parameter, public :: PG_DIAG_MESSAGE_PRIMARY = 'M'
   character(1), parameter, public :: PG_DIAG_MESSAGE_DETAIL = 'D'
   character(1), parameter, public :: PG_DIAG_MESSAGE_HINT = 'H'
   character(1), parameter, public :: PG_DIAG_STATEMENT_POSITION = 'P'
   character(1), parameter, public :: PG_DIAG_INTERNAL_POSITION = 'p'
   character(1), parameter, public :: PG_DIAG_INTERNAL_QUERY = 'q'
   character(1), parameter, public :: PG_DIAG_CONTEXT = 'W'
   character(1), parameter, public :: PG_DIAG_SCHEMA_NAME = 's'
   character(1), parameter, public :: PG_DIAG_TABLE_NAME = 't'
   character(1), parameter, public :: PG_DIAG_COLUMN_NAME = 'c'
   character(1), parameter, public :: PG_DIAG_DATATYPE_NAME = 'd'
   character(1), parameter, public :: PG_DIAG_CONSTRAINT_NAME = 'n'
   character(1), parameter, public :: PG_DIAG_SOURCE_FILE = 'F'
   character(1), parameter, public :: PG_DIAG_SOURCE_LINE = 'L'
   character(1), parameter, public :: PG_DIAG_SOURCE_FUNCTION = 'R'

end module error_message_fields
