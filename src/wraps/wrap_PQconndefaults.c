#include <libpq-fe.h>
#include <string.h>

typedef struct
{
   int siz_keyword;
   int siz_envvar;
   int siz_compiled;
   int siz_val;
   int siz_label;
   int siz_dispchar;
   int siz_dispsize;
} PQFconninfoSize ;

PQFconninfoSize *PQconndefaultPrepare(PQconninfoOption *options)
{
   options = PQconndefaults();

   PQFconninfoSize *ptr = (PQFconninfoSize *) malloc(sizeof(PQFconninfoSize));

   ptr->siz_keyword = strlen(options->keyword);
   ptr->siz_envvar = strlen(options->envvar);
   ptr->siz_compiled = strlen(options->compiled);
   ptr->siz_val = strlen(options->val);
   ptr->siz_label = strlen(options->label);
   ptr->siz_dispchar = strlen(options->dispchar);
   ptr->siz_dispsize = strlen(options->dispsize);

   PQconninfoFree(options);

   return ptr; 
}
