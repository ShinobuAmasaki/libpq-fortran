module fe_misc_m
   implicit none
   private

   public :: PQlibVersion
contains

   !> Get the current running version of libpq.
   integer function PQlibVersion()
      use, intrinsic :: iso_c_binding
      
      interface
         function c_PQ_lib_version() bind(c, name="PQlibVersion")
            import c_int
            implicit none
            integer(c_int) :: c_PQ_lib_version
         end function c_PQ_lib_version
      end interface

      PQlibVersion = c_PQ_lib_version()

   !! cf. [PostgreSQL Documentation](https://www.postgresql.org/docs/current/libpq-misc.html#LIBPQ-PQLIBVERSION)
   end function PQlibVersion


end module fe_misc_m
