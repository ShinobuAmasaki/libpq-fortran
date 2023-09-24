module libpq
   use, intrinsic :: iso_fortran_env, only: int64
   use :: error_message_fields
   use :: m_fe_connect, &
      only: PQconnectdb, PQfinish, PQstatus, PQerrorMessage, &
            PQping, PQdb, PQuser, PQhost, PQhostaddr, PQconnectdbParams, &
            PQoptions, PQtransactionStatus, PQsetdbLogin, PQpingParams, &
            PQreset, PQpass, PQport, PQprotocolVersion, PQserverVersion, &
            PQconndefaults, PQconnectStart, PQconnectStartParams, &
            PQconnectPoll, PQbackendPID, PQsocket, PQresetStart, &
            PQresetPoll, PQparameterStatus, PQconnectionNeedsPassword, &
            PQconnectionUsedPassword, PQconninfo, PQconninfoParse, &
            PQclientEncoding, PQsetClientEncoding, PQsslInUse

   
   use :: m_fe_exec, &
      only: PQexec, PQresultStatus, PQntuples, PQnfields, &
            PQgetvalue, PQclear, PQresultErrorMessage, PQfname, &
            PQfnumber, PQgetisnull, PQresultVerboseErrorMessage, &
            PQbinaryTuples, PQftablecol, PQfformat, PQfmod, PQfsize, &
            PQftable, PQftype, PQresStatus, PQgetlength, PQnparams, &
            PQparamtype, PQresultErrorField, PQcmdStatus, PQcmdTuples, &
            PQoidValue, &
            PQsendQuery, PQgetResult, PQconsumeInput, PQisBusy, &
            PQsetnonblocking, PQisnonblocking, PQflush, PQescapeLiteral, &
            PQescapeIdentifier, PQexecParams, PQprepare, PQexecPrepared, &
            PQdescribePrepared, PQdescribePortal, PQnparams, PQparamtype, &
            PQsendPrepare

   use :: m_fe_auth, only: PQencryptPasswordConn

   use :: m_fe_misc, only: PQlibVersion

   use :: t_PQconninfoOption, only: PQconninfoOption

   implicit none
   private

!------------------------------------------------------------------!
!-- Parameters
   integer(int64), parameter, public :: InvalidOid = 0

!------------------------------------------------------------------!
!-- PUBLIC Statements

!-From module m_fe_connect:
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
   public :: PQconninfoParse

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
   public :: PQsslInUse

   ! Misc.
   public :: PQclientEncoding
   public :: PQsetClientEncoding

!-From module m_fe_exec:
   public :: PQexec
   public :: PQexecParams
   public :: PQprepare
   public :: PQexecPrepared
   public :: PQdescribePrepared
   public :: PQdescribePortal
   public :: PQresultStatus
   public :: PQresultErrorMessage
   public :: PQresultVerboseErrorMessage
   public :: PQntuples
   public :: PQnfields
   public :: PQfname
   public :: PQfnumber
   public :: PQgetvalue
   public :: PQgetisnull
   public :: PQnparams
   public :: PQparamtype
   public :: PQclear
   public :: PQbinaryTuples
   public :: PQfformat
   public :: PQfmod
   public :: PQfsize
   public :: PQftablecol
   public :: PQftable
   public :: PQftype
   public :: PQresStatus
   public :: PQgetlength
   public :: PQresultErrorField
   public :: PQcmdStatus
   public :: PQcmdTuples
   public :: PQoidValue

   public :: PQsendQuery
   public :: PQsendPrepare
   public :: PQgetResult
   public :: PQconsumeInput
   public :: PQisBusy
   public :: PQsetnonblocking
   public :: PQisnonblocking
   public :: PQflush
   public :: PQescapeLiteral
   public :: PQescapeIdentifier

   ! From module m_fe_misc
   public :: PQlibVersion

   ! From module m_fe_auth
   public :: PQencryptPasswordConn

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

   public :: PQERRORS_TERSE, PQERRORS_DEFAULT, PQERRORS_VERBOSE, PQERRORS_SQLSTATE

   public :: PQSHOW_CONTEXT_NEVER, PQSHOW_CONTEXT_ERRORS, PQSHOW_ALWAYS

   public :: PGRES_EMPTY_QUERY, PGRES_COMMAND_OK, PGRES_TUPLES_OK, &
             PGRES_COPY_OUT, PGRES_COPY_IN, PGRES_BAD_RESPONSE, PGRES_NONFATAL_ERROR,  &
             PGRES_FATAL_ERROR, PGRES_COPY_BOTH, PGRES_SINGLE_TUPLE, PGRES_PIPELINE_SYNC, &
             PGRES_PIPELINE_ABORTED

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

   enum,bind(c) ! PGverbosity
      enumerator :: PQERRORS_TERSE = 0
      enumerator :: PQERRORS_DEFAULT
      enumerator :: PQERRORS_VERBOSE
      enumerator :: PQERRORS_SQLSTATE
   end enum

   enum, bind(c)  ! PQContextVisibility
      enumerator :: PQSHOW_CONTEXT_NEVER = 0
      enumerator :: PQSHOW_CONTEXT_ERRORS
      enumerator :: PQSHOW_ALWAYS
   end enum

end module libpq