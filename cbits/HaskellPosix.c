#define _GNU_SOURCE
#include <mqueue.h>
#include <netinet/in.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include "HaskellPosix.h"
#include "Rts.h"

#ifdef __GNUC__
#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#else
#define likely(x)       (x)
#define unlikely(x)     (x)
#endif

#define MAX_BYTEARRAYS 64

// Generally, this library tries to avoid wrapping POSIX functions
// in an additional function. However, for some functions whose wrappers
// unsafe FFI wrappers use unpinned ByteArray instead of Addr, the only
// way to support providing an offset (without just copying the bytes
// into pinned memory) is to use a wrapper.

// This returns an error code, not a length of written bytes. The error
// code zero means "no error".
int write_offset_loop(int fd, const char *message, HsInt offset, size_t length){
  ssize_t r;
  size_t bytesSent;
  const char* buf = message + offset;
  while(length > 0){
    if ((r = write(fd, (const void*)buf, length)) == -1) {
      return errno;
    } else {
      bytesSent = (size_t)r;
      buf = buf + bytesSent;
      length = length - bytesSent;
    }
  }
  return 0;
}
ssize_t write_offset(int fd, const char *message, HsInt offset, size_t length){
  return write(fd, (const void*)(message + offset), length);
}
ssize_t recv_offset(int socket, char *buffer, HsInt offset, size_t length, int flags) {
  return recv(socket, (void*)(buffer + offset), length, flags);
}
ssize_t send_offset(int socket, const char *buffer, HsInt offset, size_t length, int flags) {
  return send(socket, (const void*)(buffer + offset), length, flags);
}
ssize_t sendto_offset(int socket, const char *message, HsInt offset, size_t length, int flags, const struct sockaddr *dest_addr, socklen_t dest_len){
  return sendto(socket, (const void*)(message + offset), length, flags, dest_addr, dest_len);
}
ssize_t sendto_inet_offset(int socket, const char *message, HsInt offset, size_t length, int flags, uint16_t port, uint32_t inet_addr){
  struct sockaddr_in dest;
  memset(&dest, 0, sizeof(dest));
  dest.sin_family = AF_INET;
  dest.sin_addr.s_addr = inet_addr;
  dest.sin_port = port;
  return sendto(socket, (const void*)(message + offset), length, flags, (struct sockaddr*)&dest, sizeof(dest));
}
ssize_t sendto_inet_addr(int socket, const void *message, size_t length, int flags, uint16_t port, uint32_t inet_addr){
  struct sockaddr_in dest;
  memset(&dest, 0, sizeof(dest));
  dest.sin_family = AF_INET;
  dest.sin_addr.s_addr = inet_addr;
  dest.sin_port = port;
  return sendto(socket, message, length, flags, (struct sockaddr*)&dest, sizeof(dest));
}
ssize_t recvfrom_offset(int socket, char *restrict buffer, HsInt offset, size_t length, int flags, struct sockaddr *restrict address, socklen_t *restrict address_len) {
  return recvfrom(socket, (void*)(buffer + offset), length, flags, address, address_len);
}
ssize_t recvfrom_offset_peerless(int socket, char *restrict buffer, HsInt offset, size_t length, int flags) {
  return recvfrom(socket, (void*)(buffer + offset), length, flags, NULL, NULL);
}
ssize_t recvfrom_addr_peerless(int socket, void *restrict buffer, size_t length, int flags) {
  return recvfrom(socket, buffer, length, flags, NULL, NULL);
}
ssize_t recvfrom_offset_inet
  ( int socket
  , char *restrict buffer_base
  , HsInt offset
  , size_t length
  , int flags
  , struct sockaddr_in *restrict addresses
  , HsInt address_offset
  ) {
  void* buffer = (void*)(buffer_base + offset);
  struct sockaddr_in* address = addresses + address_offset;
  socklen_t address_len[1] = {sizeof(struct sockaddr_in)};
  ssize_t r = recvfrom(socket, buffer, length, flags, address, address_len);
  if (likely(address_len[0] == sizeof(struct sockaddr_in))) {
    return r;
  } else {
    fprintf(stderr, "posix-api: recvfrom_offset_bufs");
    exit(EXIT_FAILURE);
  }
}
ssize_t recvfrom_offset_inet_addr
  ( int socket
  , void *restrict buffer
  , size_t length
  , int flags
  , struct sockaddr_in *restrict addresses
  , HsInt address_offset
  ) {
  struct sockaddr_in* address = addresses + address_offset;
  socklen_t address_len[1] = {sizeof(struct sockaddr_in)};
  ssize_t r = recvfrom(socket, buffer, length, flags, address, address_len);
  if (likely(address_len[0] == sizeof(struct sockaddr_in))) {
    return r;
  } else {
    fprintf(stderr, "posix-api: recvfrom_offset_bufs");
    exit(EXIT_FAILURE);
  }
}
int setsockopt_int(int socket, int level, int option_name, int option_value) {
  return setsockopt(socket,level,option_name,&option_value,sizeof(int));
}

ssize_t sendmsg_bytearrays
  ( int sockfd
  , StgArrBytes **arrs // used for input
  , HsInt off // offset into input chunk array
  , HsInt len0 // number of chunks to send
  , HsInt offC // offset into first chunk
  , int flags
  ) {
  struct iovec bufs[MAX_BYTEARRAYS];
  HsInt len1 = len0 > MAX_BYTEARRAYS ? MAX_BYTEARRAYS : len0;
  // We must handle the first chunk specially since
  // the user can provide an offset into it.
  if(len1 > 0) {
    bufs[0].iov_base =
      (void*)(((char*)(arrs[off]->payload)) + offC);
    bufs[0].iov_len =
      (size_t)(((HsInt)(arrs[off]->bytes)) - offC);
  }
  for (HsInt i = 1; i < len1; i++) {
    bufs[i].iov_base = (void*)(arrs[off + i]->payload);
    bufs[i].iov_len = (size_t)(arrs[off + i]->bytes);
  }
  // The msg_flags field is not used when sending.
  // Consequently, we do not write to it or read from it.
  struct msghdr msg =
    { .msg_name = NULL
    , .msg_namelen = 0
    , .msg_iov = bufs
    , .msg_iovlen = (size_t)len1
    , .msg_control = NULL
    , .msg_controllen = 0
    };
  return sendmsg(sockfd,&msg,flags);
}

// The second buffer is char* instead of void* because we need
// to apply an offset to it.
ssize_t sendmsg_a
  ( int sockfd
  , void *bufA
  , size_t lenA
  , char *bufB
  , HsInt offB
  , size_t lenB
  , int flags
  ) {
  struct iovec bufs[2] =
    { { .iov_base = bufA, .iov_len = lenA }
    , { .iov_base = (void*)(bufB + offB), .iov_len = lenB }
    };
  struct msghdr msg =
    { .msg_name = NULL
    , .msg_namelen = 0
    , .msg_iov = bufs
    , .msg_iovlen = 2
    , .msg_control = NULL
    , .msg_controllen = 0
    };
  return sendmsg(sockfd,&msg,flags);
}

// The first buffer is char* instead of void* because we need
// to apply an offset to it.
ssize_t sendmsg_b
  ( int sockfd
  , char *bufA
  , HsInt offA
  , size_t lenA
  , void *bufB
  , size_t lenB
  , int flags
  ) {
  struct iovec bufs[2] =
    { { .iov_base = (void*)(bufA + offA), .iov_len = lenA }
    , { .iov_base = bufB, .iov_len = lenB }
    };
  struct msghdr msg =
    { .msg_name = NULL
    , .msg_namelen = 0
    , .msg_iov = bufs
    , .msg_iovlen = 2
    , .msg_control = NULL
    , .msg_controllen = 0
    };
  return sendmsg(sockfd,&msg,flags);
}

int recvmmsg_sockaddr_in
  ( int sockfd
  , int *lens // used for output
  , struct sockaddr_in *addrs // used for output
  , StgArrBytes **bufs // used for output
  , unsigned int vlen
  , int flags
  ) {
  // TODO: It's probably better to statically pick
  // out a maximum size for these. On the C stack,
  // the cost of doing this is basically nothing.
  // Perhaps 4096 would be a good maximum.
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

int recvmmsg_sockaddr_discard
  ( int sockfd
  , int *lens // used for output
#if __GLASGOW_HASKELL__ >= 810
  , StgArrBytes **bufs // used for output
#else
  , StgMutArrPtrs *arr // used for output
#endif
  , unsigned int vlen
  , int flags
  ) {
#if __GLASGOW_HASKELL__ < 810
  StgClosure **bufsX = arr->payload;
  StgArrBytes **bufs = (StgArrBytes**)bufsX;
#endif
  struct mmsghdr msgs[vlen];
  struct iovec vecs[vlen];
  for(unsigned int i = 0; i < vlen; i++) {
    vecs[i].iov_base = (void*)(bufs[i]->payload);
    vecs[i].iov_len = (size_t)(bufs[i]->bytes);
    // We deliberately leave msg_len and msg_flags
    // unassigned since they are set by the syscall.
    msgs[i].msg_hdr.msg_name = NULL;
    msgs[i].msg_hdr.msg_namelen = 0;
    msgs[i].msg_hdr.msg_iov = vecs + i;
    msgs[i].msg_hdr.msg_iovlen = 1;
    msgs[i].msg_hdr.msg_control = NULL;
    msgs[i].msg_hdr.msg_controllen = 0;
  }
  int r = recvmmsg(sockfd,msgs,vlen,flags,NULL);
  // TODO: Check msg_flags for MSG_TRUNC. I currently
  // do this in haskell, but it is actually easier to
  // do here.

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

ssize_t mq_send_offset(mqd_t mqdes, const char *msg, HsInt offset, size_t len, unsigned int prio){
  return mq_send(mqdes, msg + offset, len, prio);
}

int hs_get_fd_flags(int fd) {
  return fcntl(fd,F_GETFD);
}

int hs_get_fl_flags(int fd) {
  return fcntl(fd,F_GETFL);
}
