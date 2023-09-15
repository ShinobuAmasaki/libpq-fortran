module libpq
   use m_fe_connect, &
      only: PQconnectdb, PQfinish, PQstatus, PQerrorMessage
   
   use m_fe_exec, &
      only: PQexec, PQresultStatus, PQntuples, PQnfields, &
            PQgetvalue, PQclear

   implicit none
   private

   public :: PQconnectdb
   public :: PQfinish
   public :: PQstatus
   public :: PQerrorMessage

   public :: PQexec
   public :: PQresultStatus
   public :: PQntuples
   public :: PQnfields
   public :: PQgetvalue
   public :: PQclear
   
end module libpq