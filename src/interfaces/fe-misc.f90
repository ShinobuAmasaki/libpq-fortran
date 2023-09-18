module m_fe_misc
   implicit none
   private

   public :: PQlibVersion
contains


   integer function PQlibVersion()
      use, intrinsic :: iso_c_binding
      
      interface
         integer function c_PQ_lib_version() bind(c, name="PQlibVersion")
            implicit none
         end function c_PQ_lib_version
      end interface

      PQlibVersion = c_PQ_lib_version()

   end function PQlibVersion


end module m_fe_misc
