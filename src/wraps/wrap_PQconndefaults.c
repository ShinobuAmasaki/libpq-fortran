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
} PQFconninfoSize ;


PQFconninfoSize *PQconndefaultPrepare(void)
{

   PQconninfoOption *options = PQconndefaults();

   PQFconninfoSize *ptr = (PQFconninfoSize *) malloc(sizeof(PQFconninfoSize));

   ptr[0]->siz_keyword = getStrLen(options->keyword);
   ptr->siz_envvar = getStrLen(options->envvar);
   ptr->siz_compiled = getStrLen(options->compiled);
   ptr->siz_val = getStrLen(options->val);
   ptr->siz_label = getStrLen(options->label);
   ptr->siz_dispchar = getStrLen(options->dispchar);

   PQconninfoFree(options);

   return ptr; 
}


int getStrLen(const char*str) {
   return (str != NULL) ? strlen(str) : 0;
}