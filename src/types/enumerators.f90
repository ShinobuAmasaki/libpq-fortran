module enumerators
   implicit none
   
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

   enum, bind(c) ! PGpipelineStatus
      enumerator :: PQ_PIPELINE_OFF = 0
      enumerator :: PQ_PIPELINE_ON
      enumerator :: PQ_PIPELINE_ABORTED
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

   enum,bind(c) ! PGverbosity
      enumerator :: PQERRORS_TERSE = 0
      enumerator :: PQERRORS_DEFAULT
      enumerator :: PQERRORS_VERBOSE
      enumerator :: PQERRORS_SQLSTATE
   end enum

   enum, bind(c)  ! PQContextVisibility
      enumerator :: PQSHOW_CONTEXT_NEVER = 0
      enumerator :: PQSHOW_CONTEXT_ERRORS
      enumerator :: PQSHOW_CONTEXT_ALWAYS
   end enum

end module enumerators