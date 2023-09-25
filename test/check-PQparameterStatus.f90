program main
   use libpq
   use iso_c_binding

   type(c_ptr) :: conn

   print '(a)', "=== BEGIN TEST: PQparameterStatus ==="
   conn = PQconnectdb("host=localhost user=postgres dbname=postgres")
!==Add a test below===================================================!

   print *, 'server_version: ', PQparameterStatus(conn, "server_version")
   print *, 'server_encoding: ', PQparameterStatus(conn, 'server_encoding')
   print *, 'client_encoding: ', PQparameterStatus(conn, 'client_encoding')
   print *, 'application_name:', PQparameterStatus(conn, 'application_name')
   print *, 'default_transaction_read_only: ', PQparameterStatus(conn, 'default_transaction_read_only')
   print *, 'in_hot_standby: ', PQparameterStatus(conn, 'in_hot_standby')
   print *, 'is_superuser: ', PQparameterStatus(conn, 'is_superuser')
   print *, 'session_authorization: ', PQparameterStatus(conn, 'session_authorization')
   print *, 'DateStyle: ', PQparameterStatus(conn, "DateStyle")
   print *, 'IntervalStyle: ', PQparameterStatus(conn, "IntervalStyle")
   print *, 'TimeZone: ', PQparameterStatus(conn, "TimeZone")
   print *, 'integer_datetimes: ', PQparameterStatus(conn, "integer_datetimes")
   print *, 'standard_conforming_strings: ', PQparameterStatus(conn, 'standard_conforming_strings')

!==Test should be written above this line=============================!
   call PQfinish(conn)
print '(a)', "===== END TEST ====="

end program main