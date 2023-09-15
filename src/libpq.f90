module libpq
   use m_fe_connect, &
      only: PQconnectdb, PQfinish, PQstatus, PQerrorMessage
   
   use m_fe_exec, &
      only: PQexec, PQresultStatus, PQntuples, PQnfields, &
            PQgetvalue, PQclear, PQresultErrorMessage

   implicit none
   private
  
   public :: PQconnectdb
   public :: PQfinish
   public :: PQstatus
   public :: PQerrorMessage

   public :: PQexec
   public :: PQresultStatus
   public :: PQresultErrorMessage
   public :: PQntuples
   public :: PQnfields
   public :: PQgetvalue
   public :: PQclear

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

end module libpq