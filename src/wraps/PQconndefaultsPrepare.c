#include <libpq-fe.h>
#include <stdio.h>
#include <stdlib.h>
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

int getStrLen(const char*str) {
   return (str != NULL) ? strlen(str) : 0;
}


int PQconndefaultsPrepare(PQFconninfoSize **optionsizes)
{

   // PQconninfoOption構造体配列へのポインタをoptionに代入する。
   PQconninfoOption *options = PQconndefaults();

   // 配列optionsの要素数を調べる。
   int length = 0;
   int i = 0;
   while (options[i].keyword) {
      i++;
   }
   length = i;


   PQFconninfoSize *ptr = (PQFconninfoSize *) malloc(sizeof(PQFconninfoSize)*length);
   if (ptr == NULL) {
      return -1;
   }

   for (i=0; i< length; i++){

      // 各項目の文字列長を取得して、構造体の成分に代入する。
      ptr[i].siz_keyword   = getStrLen(options[i].keyword);
      ptr[i].siz_envvar    = getStrLen(options[i].envvar);
      ptr[i].siz_compiled  = getStrLen(options[i].compiled);
      ptr[i].siz_val       = getStrLen(options[i].val);
      ptr[i].siz_label     = getStrLen(options[i].label);
      ptr[i].siz_dispchar  = getStrLen(options[i].dispchar);
   }
   
   PQconninfoFree(options);

   *optionsizes = ptr;
   
   // 配列の長さを返す。
   return length; 
}

void PQconndefaultPrepareFree(PQFconninfoSize *options)
{
   free(options);
   return;
}
