module m_fe_connect
contains

   function PQconnectdb(conninfo) result(conn)
      use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_null_char
      implicit none
      
      character(*), intent(in) :: conninfo
      character(:, kind=c_char), allocatable :: c_conninfo

      type(c_ptr) :: conn

      interface
         ! Interface to PQconnectdb in interface/libpq/fe-connect.c:
         !
         ! PGconn *PQconnectdb(const char *conninfo)

         function c_PQ_connectdb(info) bind(c, name="PQconnectdb") result(conn)
            import c_ptr, c_char
            character(1, kind=c_char), intent(in) :: info(*)
            type(c_ptr) :: conn
         end function
      end interface 

      c_conninfo = conninfo//c_null_char

      conn = c_PQ_connectdb(c_conninfo)
      
   end function PQconnectdb


   function PQconnectdbParams (keywords, values, expand_dbname) result(conn)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char,&
                                             c_null_char, c_null_ptr, c_loc
      implicit none

      ! NULL終端ではない配列を受け取る
      character(*), intent(in) :: keywords(:)
      character(*), intent(in) :: values(:)

      integer :: expand_dbname
      type(c_ptr) :: conn

      integer :: max_len_key, max_len_val, i, size_keys

      interface
         function c_PQ_connectdb_params (keywords, values, expand_dbname) &
                                           bind(c, name="PQconnectdbParams") result(conn)
            import c_ptr, c_int
            ! ポインタの配列を渡すのでvalue属性は付けない。
            type(c_ptr), intent(in) :: keywords ! an array of pointers
            type(c_ptr), intent(in) :: values   ! an array of pointers
            integer(c_int), intent(in) :: expand_dbname
            type(c_ptr) :: conn
         end function c_PQ_connectdb_params
      end interface

      ! keyword,valueのペアの個数を得る。
      size_keys = size(keywords)

      ! 下で確保する文字列配列の長さを知る。
      max_len_key = 0
      max_len_val = 0
      do i = 1, size_keys
         max_len_key = max(max_len_key, len_trim(keywords(i)))
         max_len_val = max(max_len_val, len_trim(values(i)))
      end do

      block
         ! null文字をつけるために最大値よりも1だけ大きい文字列を宣言する。
         character(max_len_key+1, kind=c_char), target ::  c_keys(size_keys+1)
         character(max_len_val+1, kind=c_char), target ::  c_values(size_keys+1)

         ! ポインタの配列を宣言する。
         type(c_ptr) :: ptr_keys(size_keys+1), ptr_values(size_keys+1)

         ! c_int型整数を宣言する。
         integer(c_int) :: c_expand_dbname

         do i = 1, size_keys
            ! keywords(i)の終端にnull文字を付けて、c_keys(i)に格納する。
            c_keys(i) = trim(keywords(i))//c_null_char
            ! ポインタの配列ptr_keys(i)に、文字列c_keys(i)のアドレスを格納する。
            ptr_keys(i) = c_loc(c_keys(i))

            ! value(i)の終端にnull文字を付けて、c_values(i)に格納する。
            c_values(i) = trim(values(i))//c_null_char
            ! ptr_values(i)に、文字列c_values(i)のアドレスを格納する。
            ptr_values(i) = c_loc(c_values(i))
         end do

         ! Termination of pointer array
         c_keys(size_keys+1) = c_null_char
         c_values(size_keys+1) = c_null_char

         ptr_keys(size_keys+1) = c_null_ptr
         ptr_values(size_keys+1) = c_null_ptr

         c_expand_dbname = expand_dbname

         conn = c_PQ_connectdb_params(ptr_keys, ptr_values, c_expand_dbname)

         end block

   end function PQconnectdbParams


   function PQdb (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQdb in src/interfaces/
      interface
         function  c_PQ_db(conn) bind(c, name="PQdb")
            import c_ptr
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_db
         end function c_PQ_db
      end interface

      res => c_to_f_charpointer(c_PQ_db(conn))

   end function PQdb


   function PQuser (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQdb in src/interfaces/
      interface
         function  c_PQ_user(conn) bind(c, name="PQuser")
            import c_ptr
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_user
         end function c_PQ_user
      end interface

      res => c_to_f_charpointer(c_PQ_user(conn))

   end function PQuser


   function PQhost (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQdb in src/interfaces/
      interface
         function  c_PQ_host(conn) bind(c, name="PQhost")
            import c_ptr
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_host
         end function c_PQ_host
      end interface

      res => c_to_f_charpointer(c_PQ_host(conn))

   end function PQhost


   function PQhostaddr (conn) result(res)
      use :: character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: conn
      character(:), pointer :: res

      ! Interface PQdb in src/interfaces/
      interface
         function  c_PQ_hostaddr(conn) bind(c, name="PQhostaddr")
            import c_ptr
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_hostaddr
         end function c_PQ_hostaddr
      end interface

      res => c_to_f_charpointer(c_PQ_hostaddr(conn))

   end function PQhostaddr

   
   function PQping(conninfo) result(res)
      use, intrinsic :: iso_c_binding
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: conninfo
      character(:, kind=c_char), allocatable :: c_conninfo
      integer(int32) :: res

      interface 
         function c_PQ_ping (info) bind(c, name="PQping") result(c_res)
            import c_char, c_int
            character(1, kind=c_char), intent(in) :: info(*)
            integer(c_int) :: c_res
         end function c_PQ_ping
      end interface

      c_conninfo = conninfo//c_null_char

      res = c_PQ_ping(c_conninfo)

   end function PQping
      

   function PQstatus(conn) result(res)
      use, intrinsic :: iso_fortran_env, only: int32
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none
      type(c_ptr), intent(in) :: conn
      integer(int32) :: res 

      interface
         ! Interface to PQstatus in interface/libpq/fe-connect.c:
         ! 
         ! ConnStatusType PQstatus(const PGconn *conn)

         function c_PQ_status(conn) bind(c, name='PQstatus') result(res)
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: conn
            integer(c_int) :: res
         end function c_PQ_status
      end interface

      res = c_PQ_status(conn)

   end function PQstatus


   function PQerrorMessage(conn)
      use ::  character_pointer_wrapper
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn
      character(:, c_char), pointer :: PQerrormessage

      interface
         ! Interface to PQerrorMessage in interfaces/libpq/fe-connect.c:
         !
         ! char *PQerrorMessage(const PGconn *conn)

         function c_PQ_error_message(conn) bind(c, name='PQerrorMessage')
            import c_ptr
            type(c_ptr), intent(in), value :: conn
            type(c_ptr) :: c_PQ_error_message
         end function c_PQ_error_message
      end interface

      PQerrormessage => c_to_f_charpointer(c_PQ_error_message(conn))
   end function PQerrorMessage


   subroutine PQfinish(conn)
      use, intrinsic :: iso_c_binding
      implicit none
      
      type(c_ptr), intent(in) :: conn

      interface
         ! Interface ot PQfinish in interfaces/libpq/fe-connect.c:
         ! 
         ! void PQfinish(PGconn *conn)

         subroutine c_PQ_finish(conn) bind(c, name='PQfinish')
            import c_ptr
            type(c_ptr), intent(in), value :: conn
         end subroutine c_PQ_finish
      end interface

      call c_PQ_finish(conn)
   end subroutine PQfinish

   
end module m_fe_connect