#if defined(__linux__) || defined(__APPLE__)

#include <sys/select.h>

void fd_zero_wrap(fd_set *set)
{
   FD_ZERO(set);
   return;
}

void fd_set_wrap(int fd, fd_set*set)
{
   FD_SET(fd, set);
   return;
}

#endif