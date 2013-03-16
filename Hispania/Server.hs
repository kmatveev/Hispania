module Hispania.Server where

import Hispania.Types
import Hispania.Parser
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BS
import Blaze.ByteString.Builder

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

echoPort = 9900
maxline = 1500

--
-- The daemon infrastructure
--

server :: (Request -> Response) -> IO ()
server servlet = do
               withSocketsDo $ do
                   sock <- socket AF_INET Datagram 0
                   bindSocket sock (SockAddrInet echoPort iNADDR_ANY)
                   serveRequest servlet sock


serveRequest :: (Request -> Response) -> Socket -> IO ()
serveRequest servlet sock = do
           (mesg, remote) <- recvFrom sock maxline
           send_count <- sendTo sock (transform servlet mesg) remote
           serveRequest sock


transform :: (Request -> Response) -> BS.ByteString -> BS.ByteString
transform hlt x = (toByteString (responseBuilder res) )
        where
           req = myParse x
           res = hlt req

myParse :: BS.ByteString -> Request BS.ByteString
myParse x = case (parseOnly request x) of
              Left str -> error str
              Right req -> req

simpleResponder :: Request -> Response
simpleResponder req = Response defaultProtoVersion 200 (BS.pack "OK") (reqHeaders req) (BS.pack "new_body")


sendTestReq :: IO ()
sendTestReq = do
           withSocketsDo $ do
                   sock <- socket AF_INET Datagram 0
                   remoteHost <- inet_addr "127.0.0.1"
                   bindSocket sock (SockAddrInet (echoPort + 2) iNADDR_ANY)
                   send_count <- sendTo sock reqBytes (SockAddrInet echoPort remoteHost)
                   (mesg, client) <- recvFrom sock maxline
                   putStrLn (show mesg)
                   sClose sock

