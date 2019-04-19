#include <sys/socket.h>
#include <string.h>
#include <netinet/in.h>
#include <stdint.h>
#include "HaskellPosix.h"

// Generally, this library tries to avoid wrapping POSIX functions
// in an additional function. However, for some functions whose wrappers
// unsafe FFI wrappers use unpinned ByteArray instead of Addr, the only
// way to support providing an offset (without just copying the bytes
// into pinned memory) is to use a wrapper.

ssize_t recv_offset(int socket, char *buffer, int offset, size_t length, int flags) {
  return recv(socket, (void*)(buffer + offset), length, flags);
}
ssize_t send_offset(int socket, const char *buffer, int offset, size_t length, int flags) {
  return send(socket, (const void*)(buffer + offset), length, flags);
}
ssize_t sendto_offset(int socket, const char *message, int offset, size_t length, int flags, const struct sockaddr *dest_addr, socklen_t dest_len){
  return sendto(socket, (const void*)(message + offset), length, flags, dest_addr, dest_len);
}
ssize_t sendto_inet_offset(int socket, const char *message, int offset, size_t length, int flags, uint16_t port, uint32_t inet_addr){
  struct sockaddr_in dest;
  memset(&dest, 0, sizeof(dest));
  dest.sin_family = AF_INET;
  dest.sin_addr.s_addr = inet_addr;
  dest.sin_port = port;
  return sendto(socket, (const void*)(message + offset), length, flags, (struct sockaddr*)&dest, sizeof(dest));
}
ssize_t recvfrom_offset(int socket, char *restrict buffer, int offset, size_t length, int flags, struct sockaddr *restrict address, socklen_t *restrict address_len) {
  return recvfrom(socket, (void*)(buffer + offset), length, flags, address, address_len);
}
int setsockopt_int(int socket, int level, int option_name, int option_value) {
  return setsockopt(socket,level,option_name,&option_value,sizeof(int));
}
