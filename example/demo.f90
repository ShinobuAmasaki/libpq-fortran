program demo
   use :: libpq
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only:stdout=>output_unit, stdin=>input_unit

   integer :: i

   type(c_ptr) :: conn, res
   character(:, kind=c_char), allocatable :: command, conninfo
   
   character(256) :: str, host, dbname, user, password
   integer :: port

   print *, '=== INPUT Database Information ==='
   print *, '=== type "q" for quit          ==='
   write (stdout, '(a)', advance='no') "Hostname: "
   read  (stdin, *) host

   if (host == 'q') stop

   port = 5432

   write (stdout, '(a)', advance='no') "dbname: "
   read  (stdin, *) dbname
   if (dbname == 'q') stop

   write (stdout, '(a)', advance='no') "user: "
   read  (stdin, *) user
   if (user == 'q') stop

   write (stdout, '(a)', advance='no') "password: "
   read  (stdin, *) password
   if (password == 'q') stop
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

   ! データベースクラスタ内のデータベース名を取得する
   ! The query to retrieve the names of databases within a database cluster
   command = "select datname from pg_database;"

   res = PQexec(conn, command)
   if (PQstatus(conn) /= 0 ) then
      print *, PQerrorMessage(conn)
   end if

   print *, "=== Query Result==="
   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   print *, "=== Available database names ==="
   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)
   end do

   call PQclear(res)
   call PQfinish(conn)


end program demo