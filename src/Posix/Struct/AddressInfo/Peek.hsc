#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

{-# language DataKinds #-}

-- | Accessors for reading from @struct addrinfo@:
--
-- > struct addrinfo {
-- >     int              ai_flags;
-- >     int              ai_family;
-- >     int              ai_socktype;
-- >     int              ai_protocol;
-- >     socklen_t        ai_addrlen;
-- >     struct sockaddr *ai_addr;
-- >     char            *ai_canonname;
-- >     struct addrinfo *ai_next;
-- > };
module Posix.Struct.AddressInfo.Peek
  ( flags
  , family
  , socketType
  , protocol
  , addressLength
  , address
  , next
  ) where

import Posix.Socket.Types (AddressInfoFlags(..),SocketAddress,Family,Type,AddressInfo,Protocol)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

-- | Get @ai_flags@.
flags :: Ptr AddressInfo -> IO AddressInfoFlags
flags ptr = #{peek struct addrinfo, ai_flags} ptr

-- | Get @ai_family@.
family :: Ptr AddressInfo -> IO Family
family ptr = #{peek struct addrinfo, ai_family} ptr

-- | Get @ai_socktype@.
socketType :: Ptr AddressInfo -> IO Type
socketType ptr = #{peek struct addrinfo, ai_socktype} ptr

-- | Get @ai_protocol@.
protocol :: Ptr AddressInfo -> IO Protocol
protocol ptr = #{peek struct addrinfo, ai_protocol} ptr

-- | Get @ai_addrlen@.
addressLength :: Ptr AddressInfo -> IO CInt
addressLength ptr = #{peek struct addrinfo, ai_addrlen} ptr

-- | Get @ai_addr@.
address :: Ptr AddressInfo -> IO (Ptr SocketAddress)
address ptr = #{peek struct addrinfo, ai_addr} ptr

-- | Get @ai_next@.
next :: Ptr AddressInfo -> IO (Ptr AddressInfo)
next ptr = #{peek struct addrinfo, ai_next} ptr
