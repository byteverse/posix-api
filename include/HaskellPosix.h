#include <sys/types.h>
#include <sys/socket.h>
#include <stdint.h>
#include "Rts.h"

ssize_t recv_offset(int socket, char *buffer, int offset, size_t length, int flags);
ssize_t send_offset(int socket, const char *buffer, int offset, size_t length, int flags);

ssize_t sendto_offset(int socket, const char *message, int offset, size_t length, int flags, const struct sockaddr *dest_addr, socklen_t dest_len);
ssize_t sendto_inet_offset(int socket, const char *message, int offset, size_t length, int flags, uint16_t port, uint32_t inet_addr);
ssize_t recvfrom_offset(int socket, char *restrict buffer, int offset, size_t length, int flags, struct sockaddr *restrict address, socklen_t *restrict address_len);

int setsockopt_int(int socket, int level, int option_name, int option_value);

int recvmmsg_sockaddr_in (int sockfd , int *lens , struct sockaddr_in *addrs , StgMutArrPtrs *arr , unsigned int vlen , int flags);
int recvmmsg_sockaddr_discard (int sockfd , int *lens , StgMutArrPtrs *arr , unsigned int vlen , int flags);

