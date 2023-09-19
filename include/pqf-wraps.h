#ifdef __cplusplus
extern "C"
{
#endif

#include <stdio.h>
#include <libpq-fe.h>

typedef struct
{
   int siz_keyword;
   int siz_envvar;
   int siz_compiled;
   int siz_val;
   int siz_label;
   int siz_dispchar;
} PQFconninfoSize ;


extern int getStrLen(const char*str);

#ifdef __cplusplus
}
#endif