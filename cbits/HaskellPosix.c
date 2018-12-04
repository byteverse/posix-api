#include <sys/socket.h>
#include "HaskellPosix.h"

// Generally, this library tries to avoid wrapping POSIX functions
// in an additional function. However, for some functions whose wrappers
// unsafe FFI wrappers use unpinned ByteArray instead of Addr, the only
// way to support providing an offset (without just copying the bytes
// into pinned memory) is to use a wrapper.

ssize_t recv_offset(int socket, char *buffer, int offset, size_t length, int flags) {
  recv(socket, (void*)(buffer + offset), length, flags);
}
ssize_t send_offset(int socket, const char *buffer, int offset, size_t length, int flags) {
  send(socket, (const void*)(buffer + offset), length, flags);
}
