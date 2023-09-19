module libpq
   use m_fe_connect, &
      only: PQconnectdb, PQfinish, PQstatus, PQerrorMessage, &
            PQping, PQdb, PQuser, PQhost, PQhostaddr, PQconnectdbParams, &
            PQoptions, PQtransactionStatus, PQsetdbLogin, PQpingParams, &
            PQreset, PQpass, PQport, PQprotocolVersion, PQserverVersion, &
            PQconndefaults, PQconnectStart, PQconnectStartParams, &
            PQconnectPoll, PQbackendPID, PQsocket, PQresetStart, &
            PQresetPoll, PQparameterStatus, PQconnectionNeedsPassword, &
            PQconnectionUsedPassword, PQconninfo

   
   use m_fe_exec, &
      only: PQexec, PQresultStatus, PQntuples, PQnfields, &
            PQgetvalue, PQclear, PQresultErrorMessage, PQfname, &
            PQfnumber

   use m_fe_misc, only: PQlibVersion

   use t_PQconninfoOption, only: PQconninfoOption

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
   public :: PQconndefaults
   public :: PQconnectStart
   public :: PQconnectStartParams
   public :: PQconnectPoll
   public :: PQresetPoll
   public :: PQresetStart
   public :: PQconninfo

   ! Connction Status Functions
   public :: PQdb
   public :: PQuser
   public :: PQpass
   public :: PQhost
   public :: PQport
   public :: PQhostaddr
   public :: PQoptions
   public :: PQstatus
   public :: PQtransactionStatus
   public :: PQserverVersion
   public :: PQprotocolVersion
   public :: PQsocket
   public :: PQbackendPID
   public :: PQparameterStatus
   public :: PQconnectionNeedsPassword
   public :: PQconnectionUsedPassword

   ! From module m_fe_exec:
   public :: PQexec
   public :: PQresultStatus
   public :: PQresultErrorMessage
   public :: PQntuples
   public :: PQnfields
   public :: PQfname
   public :: PQfnumber
   public :: PQgetvalue
   public :: PQclear

   ! From module m_fe_misc
   public :: PQlibVersion

   ! Derived types
   public :: PQconninfoOption

   ! Enumerators
   public :: CONNECTION_OK, CONNECTION_BAD, &
             CONNECTION_MADE, &
             CONNECTION_AWAITING_RESPONSE, &
             CONNECTION_AUTH_OK, & 
             CONNECTION_SETENV, &
             CONNECTION_SSL_STARTUP, &
             CONNECTION_NEEDED, &
             CONNECTION_CHECK_WRITABLE, &
             CONNECTION_CONSUME, &
             CONNECTION_GSS_STARTUP, &
             CONNECTION_CHECK_TARGET, &
             CONNECTION_CHECK_STANDBY

   public :: PQPING_OK, PQPING_REJECT, PQPING_NO_RESPONSE, &
             PQPING_NO_ATTEMPT
   public :: PQTRANS_IDLE, PQTRANS_ACTIVE, PQTRANS_INTRANS, &
             PQTRANS_INERROR, PQTRANS_UNKNOWN
   public :: PGRES_POLLING_FAILED, PGRES_POLLING_READING, &
             PGRES_POLLING_WRITING, PGRES_POLLING_OK, &
             PGRES_POLLING_ACTIVE


   !------------------------------------------------------------------!
   !-- ENUMERATOR declarations

   enum, bind(c) ! ConnStatusType in src/interfaces/libpq/libpq-fe.h
      enumerator :: CONNECTION_OK = 0 
      enumerator :: CONNECTION_BAD
      enumerator :: CONNECTION_MADE
      enumerator :: CONNECTION_AWAITING_RESPONSE
      enumerator :: CONNECTION_AUTH_OK
      enumerator :: CONNECTION_SETENV
      enumerator :: CONNECTION_SSL_STARTUP
      enumerator :: CONNECTION_NEEDED
      enumerator :: CONNECTION_CHECK_WRITABLE
      enumerator :: CONNECTION_CONSUME
      enumerator :: CONNECTION_GSS_STARTUP
      enumerator :: CONNECTION_CHECK_TARGET
      enumerator :: CONNECTION_CHECK_STANDBY
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

   enum,bind(c) ! PostgresPollingStatusType
      enumerator :: PGRES_POLLING_FAILED = 0
      enumerator :: PGRES_POLLING_READING
      enumerator :: PGRES_POLLING_WRITING
      enumerator :: PGRES_POLLING_OK
      enumerator :: PGRES_POLLING_ACTIVE
   end enum

end module libpq