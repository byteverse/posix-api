#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

{-# language DataKinds #-}

-- | Accessors for reading from @struct sockaddr_in@:
--
-- > struct sockaddr_in {
-- >     sa_family_t    sin_family; /* address family: AF_INET */
-- >     in_port_t      sin_port;   /* port in network byte order */
-- >     struct in_addr sin_addr;   /* internet address */
-- > };
module Posix.Struct.SocketAddressInternet.Peek
  ( family
  , port
  , address
  ) where

import Posix.Socket.Types (SocketAddressInternet,Family)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import Data.Word (Word16,Word32)
import System.ByteOrder (Fixed,ByteOrder(BigEndian))

-- | Get @sin_family@.
family :: Ptr SocketAddressInternet -> IO Family
family = #{peek struct sockaddr_in, sin_family}

-- | Get @in_port_t@.
port :: Ptr SocketAddressInternet -> IO (Fixed 'BigEndian Word16)
port = #{peek struct sockaddr_in, sin_port}

-- | Get @sin_addr.saddr@. This works on Linux because @struct in_addr@ has
-- a single 32-bit field. I do not know how to perform this in a portable way
-- with hsc2hs.
address :: Ptr SocketAddressInternet -> IO (Fixed 'BigEndian Word32)
address = #{peek struct sockaddr_in, sin_addr}
