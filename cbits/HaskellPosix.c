#define _GNU_SOURCE
#include <sys/socket.h>
#include <string.h>
#include <netinet/in.h>
#include <stdint.h>
#include "HaskellPosix.h"
#include "Rts.h"
// #include "rts/storage/Closures.h"

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
int recvmmsg_sockaddr_in
  ( int sockfd
  , int *lens // used for output
  , struct sockaddr_in *addrs // used for output
  , StgMutArrPtrs *arr // used for output
  , unsigned int vlen
  , int flags
  ) {
  StgClosure **bufsX = arr->payload;
  StgArrBytes **bufs = (StgArrBytes**)bufsX;
  struct mmsghdr msgs[vlen];
  struct iovec vecs[vlen];
  for(unsigned int i = 0; i < vlen; i++) {
    vecs[i].iov_base = (void*)(bufs[i]->payload);
    vecs[i].iov_len = (size_t)(bufs[i]->bytes);
    // We deliberately leave msg_len unassigned.
    msgs[i].msg_hdr.msg_name = addrs + i;
    msgs[i].msg_hdr.msg_namelen = sizeof(struct sockaddr_in);
    msgs[i].msg_hdr.msg_iov = vecs + i;
    msgs[i].msg_hdr.msg_iovlen = 1;
    msgs[i].msg_hdr.msg_control = NULL;
    msgs[i].msg_hdr.msg_controllen = 0;
    msgs[i].msg_hdr.msg_flags = flags;
  }
  int r = recvmmsg(sockfd,msgs,vlen,flags,NULL);
  // If no errors occurred, copy all of the lengths into the
  // length buffer. This copy makes me feel a little sad.
  // It is the only copy in a wrapper that otherwise is
  // able to share buffers perfectly between Haskell and C.
  if(r > (-1)) {
    for(int i = 0; i < r; i++) {
      lens[i] = msgs[i].msg_len;
    }
  }
  return r;
}

