module libpq
   use m_fe_connect, &
      only: PQconnectdb, PQfinish, PQstatus, PQerrorMessage, &
            PQping, PQdb, PQuser, PQhost, PQhostaddr, PQconnectdbParams, &
            PQoptions, PQtransactionStatus, PQsetdbLogin, PQpingParams, &
            PQreset

   
   use m_fe_exec, &
      only: PQexec, PQresultStatus, PQntuples, PQnfields, &
            PQgetvalue, PQclear, PQresultErrorMessage, PQfname

   implicit none
   private

   !------------------------------------------------------------------!
   !-- PUBLIC Statements
  
   ! From module m_fe_connect:
      ! Database Connection Control Functions
      public :: PQconnectdb
      public :: PQconnectdbParams
      public :: PQfinish
      public :: PQerrorMessage
      public :: PQping
      public :: PQsetdbLogin
      public :: PQpingParams
      public :: PQreset

      ! Connction Status Functions
      public :: PQdb
      public :: PQuser
      public :: PQhost
      public :: PQhostaddr
      public :: PQoptions
      public :: PQstatus
      public :: PQtransactionStatus

   ! From module m_fe_exec:
   public :: PQexec
   public :: PQresultStatus
   public :: PQresultErrorMessage
   public :: PQntuples
   public :: PQnfields
   public :: PQfname
   public :: PQgetvalue
   public :: PQclear

   ! Enumerators
   public :: CONNECTION_OK, CONNECTION_BAD
   public :: PQPING_OK, PQPING_REJECT, PQPING_NO_RESPONSE, &
             PQPING_NO_ATTEMPT
   public :: PQTRANS_IDLE, PQTRANS_ACTIVE, PQTRANS_INTRANS, &
             PQTRANS_INERROR, PQTRANS_UNKNOWN


   !------------------------------------------------------------------!
   !-- ENUMERATOR declarations

   enum, bind(c) ! ConnStatusType in src/interfaces/libpq/libpq-fe.h
      enumerator :: CONNECTION_OK = 0 
      enumerator :: CONNECTION_BAD
   end enum

   enum, bind(c) ! ExecStatusType in src/interfaces/libpq/libpq-fe.h
      enumerator :: PGRES_EMPTY_QUERY = 0
      enumerator :: PGRES_COMMAND_OK
      enumerator :: PGRES_TUPLES_OK
      enumerator :: PGRES_COPY_OUT
      enumerator :: PGRES_COPY_IN
      enumerator :: PGRES_BAD_RESPONSE
      enumerator :: PGRES_NONFATAL_ERROR
      enumerator :: PGRES_FATAL_ERROR
      enumerator :: PGRES_COPY_BOTH
      enumerator :: PGRES_SINGLE_TUPLE
      enumerator :: PGRES_PIPELINE_SYNC
      enumerator :: PGRES_PIPELINE_ABORTED
   end enum
   
   enum, bind(c) ! PGPing in src/interfaces/libpq/libpq-fe.h
      enumerator :: PQPING_OK = 0
      enumerator :: PQPING_REJECT
      enumerator :: PQPING_NO_RESPONSE
      enumerator :: PQPING_NO_ATTEMPT
   end enum 

   enum, bind(c) ! PGTransactionStatusType
      enumerator :: PQTRANS_IDLE = 0
      enumerator :: PQTRANS_ACTIVE
      enumerator :: PQTRANS_INTRANS
      enumerator :: PQTRANS_INERROR
      enumerator :: PQTRANS_UNKNOWN
   end enum

end module libpq