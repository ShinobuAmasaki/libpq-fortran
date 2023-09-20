#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libpq-fe.h>

#include "pqf-wraps.h"

int PQconninfoParsePrepare(const char *conninfo, char **errmsg, int **errlen, PQFconninfoSize**optionsizes)
{
   int length;
   char* errmsg_local;

   // 接続情報を解析し、オプション情報を取得する。
   PQconninfoOption *options = PQconninfoParse(conninfo, &errmsg_local);

   length = 0;
   *errlen = 0;

   if (options == NULL) {
      // エラーが生じた場合
      if (errmsg_local != NULL) {
         // エラーメッセージを引数に返す。
         *errmsg = errmsg_local;
         // エラーメッセージの長さを計算して引数に返す。
         *errlen = getStrLen(errmsg_local);
      }
      return length;
   }

   if (errmsg_local != NULL ) {
      free(errmsg_local);
   }

   // 配列optionsの要素数を調べる。
   length = 0;
   int i = 0;
   while (options[i].keyword) {
      i++;
   }
   length = i;

   // オプション情報の文字列長を取得し、PQFconninfoSize構造体に格納する。
   PQFconninfoSize *ptr = (PQFconninfoSize *) malloc(sizeof(PQFconninfoSize)*length );
   if (ptr == NULL) {
      return -1;
   }

   for (int i=0; i<length; i++) {
      // 各項目の文字列長を取得して、構造体の成分に代入する。
      ptr[i].siz_keyword   = getStrLen(options[i].keyword);
      ptr[i].siz_envvar    = getStrLen(options[i].envvar);
      ptr[i].siz_compiled  = getStrLen(options[i].compiled);
      ptr[i].siz_val       = getStrLen(options[i].val);
      ptr[i].siz_label     = getStrLen(options[i].label);
      ptr[i].siz_dispchar  = getStrLen(options[i].dispchar);
   }

   // オプション情報を解放する。
   PQconninfoFree(options);
   *optionsizes = ptr;

   return length;
}

void PQconninfoParsePrepareFree(PQFconninfoSize*options)
{
   // オプション情報のメモリを解放する。
   free(options);
   return;
}