#include "pqf-wraps.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libpq-fe.h>

int PQsslAttributeNamesPrepareSize (PGconn *conn)
{
   const char* const* result = PQsslAttributeNames(conn);

   int i = 0;
   for (i = 0; result[i]!= NULL; i++) {
   }

   return i;
}

void PQsslAttributeNamesPrepareLengths (PGconn *conn, int **lengths)
{
   const char* const* result = PQsslAttributeNames(conn);

   int i;

   for (i = 0; result[i]!=NULL; i++){
      (*lengths)[i] = getStrLen(result[i]);
   }

   return;
}