program main
   use :: libpq
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only:stdout=>output_unit, stdin=>input_unit

   integer :: i

   type(c_ptr) :: conn, res
   character(:, kind=c_char), allocatable :: sql, conninfo
   
   character(256) :: str, host, dbname, user, password
   integer :: port

   print *, '=== INPUT Database Information ==='
   write (stdout, '(a)', advance='no') "Hostname: "
   read  (stdin, *) host

   write (stdout, '(a)', advance='no') "Port: "
   read  (stdin, *) port
   if (port < 1 .or. 65535 < port) then
      print *, "Error: Invalid port number."
      error stop
   end if

   write (stdout, '(a)', advance='no') "dbname: "
   read  (stdin, *) dbname

   write (stdout, '(a)', advance='no') "user: "
   read  (stdin, *) user

   write (stdout, '(a)', advance='no') "password: "
   read  (stdin, *) password
   print *, "=== END INPUT ==="

   write(str, '(a, i0, a)') &
      "host="//trim(adjustl(host))// &
      " port=", port, &
      " dbname="//trim(adjustl(dbname))// &
      " user="//trim(adjustl(user))// &
      " password="//trim(adjustl(password))
     
   conninfo = str

   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if 

   ! データベースクラスタ内のデータベース名を取得するクエリ
   sql = "select datname from pg_database;"

   res = PQexec(conn, sql)
   if (PQstatus(conn) /= 0 ) then
      print *, PQerrorMessage(conn)
   end if

   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)
   end do

   call PQclear(res)
   call PQfinish(conn)


end program main