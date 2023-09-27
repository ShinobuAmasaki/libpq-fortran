module fe_auth_m
   implicit none
   private

   public :: PQencryptPasswordConn


contains

   ! This subroutine encrypts a password using PostgreSQL's PQencryptPasswordConn.
   ! Inputs:
   !   - conn: PostgreSQL connection pointer
   !   - passwd: Password to be encrypted
   !   - user: User associated with the password
   !   - algorithm: Encryption algorithm to use ('md5' or 'scram-sha-256')
   ! Output:
   !   - encrypted: Encrypted password

   subroutine PQencryptPasswordConn (conn, passwd, user, algorithm, encrypted) 

      use :: character_operations_m
      use :: fe_exec_m
      use, intrinsic :: iso_fortran_env
      use, intrinsic :: iso_c_binding
      implicit none
      
      ! Input paramters
      type(c_ptr), intent(in) :: conn
      character(*), intent(in) :: passwd
      character(*), intent(in) :: user
      character(*), intent(in) :: algorithm
      character(*), intent(out):: encrypted

      ! Local variables
      character(:), allocatable :: buff
      type(c_ptr) :: cptr
      character(:, kind=c_char), allocatable, target :: c_passwd, c_user, c_algorithm


      interface
         function c_PQ_encrypt_password_conn (conn, passwd, user, algorithm) bind(c, name="PQencryptPasswordConn")
            import c_ptr, c_char
            implicit none
            type(c_ptr), intent(in), value :: conn
            character(1, kind=c_char), intent(in) :: passwd(*)
            character(1, kind=c_char), intent(in) :: user(*)
            character(1, kind=c_char), intent(in) :: algorithm(*)
            type(c_ptr) :: c_PQ_encrypt_password_conn
         end function c_PQ_encrypt_password_conn
      end interface
      
      ! Initialize 'encrypted'.
      encrypted = ''

      ! Convert Fortran strings to C-style strings.
      c_passwd    = trim(adjustl(passwd))//c_null_char
      c_user      = trim(adjustl(user))//c_null_char
      c_algorithm = trim(adjustl(algorithm))//c_null_char


      ! Call the C function to encrypt the password.
      cptr = c_PQ_encrypt_password_conn(conn, c_passwd, c_user, c_algorithm)
      
      ! Convert the C result to Fortran string.
      call c_char_to_f_string(cptr, buff)

      ! Store the encrypted password.
      encrypted = buff

      ! Free the C memory
      call PQfreemem(cptr)

   end subroutine PQencryptPasswordConn


end module fe_auth_m