#include <sys/types.h>

ssize_t recv_offset(int socket, char *buffer, int offset, size_t length, int flags);
ssize_t send_offset(int socket, const char *buffer, int offset, size_t length, int flags);
