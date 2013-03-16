module Hispania.Transport where

import Hispania.Types
import Hispania.Parser

import Data.List
import Data.Maybe
import Data.Word
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString as ATP
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD

maxline = 1500

newtype Direction = Direction (SockAddr, Maybe SockAddr) deriving Show

source :: Direction -> Maybe SockAddr
source (Direction dir) = snd dir

destination :: Direction -> SockAddr
destination (Direction dir) = fst dir



reverseDirection :: Direction -> Direction
reverseDirection (Direction (dest, Just src)) = Direction (src, Just dest)
reverseDirection (Direction (_, Nothing)) = error "Cannot reverse direction without a source"

newtype SocketPoolTCP = SocketPoolTCP {tcpInfo :: [((SockAddr, SockAddr), Socket)]}

newtype SocketPoolUDP = SocketPoolUDP {udpInfo :: [(SockAddr, Socket)]}

class SocketPool a where

   getExistingForOut :: Direction -> a -> Maybe Socket

instance SocketPool SocketPoolTCP where 

   getExistingForOut direction (SocketPoolTCP storage) = fmap snd (find ((matchDirection direction) . fst) storage)
     where
       matchDirection (Direction (dest, Just src)) (remote, local) = (dest == remote) && (src == local)
       matchDirection (Direction (dest, Nothing )) (remote, local) = (dest == remote)


instance SocketPool SocketPoolUDP where 

   getExistingForOut direction (SocketPoolUDP storage) = fmap snd (find condition storage)
       where
          condition = case (source direction) of 
                        Nothing -> const True
                        Just addr -> ((addr==) . fst)


addSocketUDP :: SockAddr -> Socket -> TransportLayer -> TransportLayer
addSocketUDP local socket transportLayer = TransportLayer (SocketPoolUDP ((local, socket):(udpInfo (poolUDP transportLayer)))) (poolTCP transportLayer)


addSocketTCP :: SockAddr -> SockAddr -> Socket -> TransportLayer -> TransportLayer
addSocketTCP local remote socket transportLayer = TransportLayer (poolUDP transportLayer) (SocketPoolTCP (((remote, local), socket):(tcpInfo (poolTCP transportLayer))))

data TransportLayer = TransportLayer {poolUDP :: SocketPoolUDP, poolTCP :: SocketPoolTCP}

clearTransportLayer :: TransportLayer
clearTransportLayer = TransportLayer (SocketPoolUDP []) (SocketPoolTCP [])


receive:: TransportLayer -> IO (BS.ByteString, Direction, Transport)
receive transportLayer = 
      do
        (mesg, remote) <- recvFrom sock maxline
        return (mesg, Direction (local, Just remote), UDP)
    where
      pool = udpInfo (poolUDP transportLayer)
      entry = head pool
      sock  = snd entry
      local = fst entry

getSocket :: Transport -> Direction -> TransportLayer -> IO (Socket, Maybe TransportLayer)

getSocket TCP direction transportLayer = case existing of
                                           Just socket -> return (socket, Just transportLayer)
                                           Nothing -> error "not found"
   where
      existing = getExistingForOut direction (poolTCP transportLayer)

getSocket UDP direction transportLayer = case existing of
                                          Just socket -> return (socket, Just transportLayer)
                                          Nothing -> error "not found"
    where
      existing = getExistingForOut direction (poolUDP transportLayer)


sendRawTo :: BS.ByteString -> Direction -> Transport -> TransportLayer -> IO (Maybe TransportLayer)
sendRawTo bytes direction transport transportLayer =
         let remote = (destination direction) in
         do
           putStrLn ("Sending to:" ++ (show remote))
           (sock, updatedTransportLayer) <- getSocket transport direction transportLayer
           send_count <- sendTo sock bytes remote
           return updatedTransportLayer


-- getSocketForOut transportLayer poolAccess direction = fromJust existing 
--   where
--     pool = poolAccess transportLayer
--     existing = getExistingForOut direction pool

getPort :: SockAddr -> Word16
getPort (SockAddrInet (PortNum port) host) = port
getPort (SockAddrInet6 (PortNum port) flowinfo host scopeid) = port

getHost :: SockAddr -> BS.ByteString
getHost (SockAddrInet port host) = BS.pack (show host)
getHost (SockAddrInet6 port flowinfo host scopeid) = BS.pack (show host)
