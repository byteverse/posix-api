#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

{-# language DataKinds #-}

-- | Setters for assigning fields of @struct addrinfo@:
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
module Posix.Struct.AddressInfo.Poke
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
import Foreign.Storable (pokeByteOff)

-- | Get @ai_flags@.
flags :: Ptr AddressInfo -> AddressInfoFlags -> IO ()
flags ptr = #{poke struct addrinfo, ai_flags} ptr

-- | Get @ai_family@.
family :: Ptr AddressInfo -> Family -> IO ()
family ptr = #{poke struct addrinfo, ai_family} ptr

-- | Get @ai_socktype@.
socketType :: Ptr AddressInfo -> Type -> IO ()
socketType ptr = #{poke struct addrinfo, ai_socktype} ptr

-- | Get @ai_protocol@.
protocol :: Ptr AddressInfo -> Protocol -> IO ()
protocol ptr = #{poke struct addrinfo, ai_protocol} ptr

-- | Get @ai_addrlen@.
addressLength :: Ptr AddressInfo -> CInt -> IO ()
addressLength ptr = #{poke struct addrinfo, ai_addrlen} ptr

-- | Get @ai_addr@.
address :: Ptr AddressInfo -> Ptr SocketAddress -> IO ()
address ptr = #{poke struct addrinfo, ai_addr} ptr

-- | Get @ai_next@.
next :: Ptr AddressInfo -> Ptr AddressInfo -> IO ()
next ptr = #{poke struct addrinfo, ai_next} ptr
