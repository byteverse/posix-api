#include <sys/types.h>
#include <sys/socket.h>

ssize_t recv_offset(int socket, char *buffer, int offset, size_t length, int flags);
ssize_t send_offset(int socket, const char *buffer, int offset, size_t length, int flags);

ssize_t sendto_offset(int socket, const char *message, int offset, size_t length, int flags, const struct sockaddr *dest_addr, socklen_t dest_len);
ssize_t recvfrom_offset(int socket, char *restrict buffer, int offset, size_t length, int flags, struct sockaddr *restrict address, socklen_t *restrict address_len);



