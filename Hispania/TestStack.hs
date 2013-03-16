module Hispania.TestStack where

import Hispania.Stack
import Hispania.Types
import Hispania.Transport
import Hispania.Parser

import Hispania.TestTypes

import Data.ByteString.Char8 as BS
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Data.Maybe
import Data.Word
import qualified Data.Map as Map

import System.IO.Unsafe
import System.IO

-- |-------------------------------------------------------------
-- |
-- | Common part: configuration
-- |
-- |-------------------------------------------------------------

data TestConfig = TestConfig{ testLocalPort::PortNumber, testRemotePort::PortNumber, testTransport::Transport}

defaultConfig = TestConfig (fromIntegral localPort) (fromIntegral remotePort) UDP

localPort  = 5657
remotePort = 5698


-- |-------------------------------------------------------------
-- |
-- | Common part: test framework, initial request handlers.
-- |
-- |-------------------------------------------------------------

simpleResponder :: RequestHandler
simpleResponder req serverCtx = respond serverCtx response
  where
    response = respondTo req 200

printingHandler :: RequestHandler
printingHandler req serverCtx stack = do
                                       System.IO.putStrLn "Received request"
                                       hFlush stdout
                                       return stack

data TestEnv = TestEnv {localSock::Socket, localSockAddr::SockAddr, remoteSock::Socket, remoteSockAddr::SockAddr, testedStack::Stack}

prepareTest :: TestConfig -> RequestHandler -> IO TestEnv
prepareTest config handler = do
                       localSock <- socket AF_INET Datagram 0
                       localHostAddr <- inet_addr localHostName
                       let localSockAddr = (SockAddrInet (testLocalPort config) localHostAddr)
                       bindSocket localSock localSockAddr
                       remoteSock <- socket AF_INET Datagram 0
                       remoteHostAddr <- inet_addr localHostName
                       let remoteSockAddr = (SockAddrInet (testRemotePort config) remoteHostAddr)
                       bindSocket remoteSock remoteSockAddr
                       let cleanStack = newStack handler
                       let initializedStack = cleanStack{transportLayer = (addSocketUDP localSockAddr localSock (transportLayer cleanStack))}
                       return (TestEnv localSock localSockAddr remoteSock remoteSockAddr initializedStack)
    where
       remoteHostName = "127.0.0.1"
       localHostName = "127.0.0.1"
       message = incomingRegisterStr

finishTest :: TestEnv -> IO ()
finishTest env = do
                  sClose (localSock env)
                  sClose (remoteSock env)


inject :: TestEnv -> BS.ByteString -> IO Int
inject env msg = sendTo (remoteSock env) msg (localSockAddr env)

handleAll :: TestEnv -> IO TestEnv
handleAll env = serveOne (testedStack env) >>= \x -> return (env{testedStack = x})

catchOutgoing :: TestEnv -> IO (BS.ByteString, SockAddr)
catchOutgoing env = recvFrom (remoteSock env) 1500


-- |-------------------------------------------------------------
-- |
-- | Tests for server functionality
-- |
-- |-------------------------------------------------------------


incomingRegisterStr = "REGISTER sip:user@host SIP/2.0\r\nFrom: User <sip:user@domain.dom>\r\nTo: User <sip:user@domain.dom>\r\nCall-ID: 123321\r\nVia: SIP/2.0/UDP 127.0.0.1:5060;branch=123123123123\r\n\r\n"
incomingRegister = BS.pack incomingRegisterStr

testIncomingRegister = do
                         testEnv <- prepareTest defaultConfig printingHandler
                         sendCount <- inject testEnv message
                         updatedEnv <- handleAll testEnv
                         finishTest updatedEnv
   where
     message = incomingRegister


testIncomingRegister2 = do
                         testEnv <- prepareTest defaultConfig simpleResponder
                         sendCount <- inject testEnv message
                         updatedEnv <- handleAll testEnv
                         (resp, to) <- catchOutgoing testEnv
                         System.IO.putStrLn (BS.unpack resp)
                         finishTest updatedEnv
   where
     message = incomingRegister


-- |-------------------------------------------------------------
-- |
-- | Tests for client functionality
-- |
-- |-------------------------------------------------------------

testResponseHandler resp = unsafePerformIO (Prelude.putStrLn (show resp))

emptyResponseHandler :: ResponseHandler
emptyResponseHandler res = ()


testReqURI = (RawURI (BS.pack "sip") (BS.pack "127.0.0.1") )

testOutgoingReq = Request INVITE testReqURI defaultProtoVersion [testFrom, testTo, testCallId] ()


testFromURI = (RawURI (BS.pack "sip") (BS.pack "127.0.0.1") )
testToURI = (RawURI (BS.pack "sip") (BS.pack "127.0.0.1") )

initStack :: Stack -> IO Stack
initStack stack = do
              sock <- socket AF_INET Datagram 0
              bindSocket sock localAddr
              return (stack{transportLayer=(addSocketUDP localAddr sock (transportLayer stack))})
   where
     localAddr = SockAddrInet localPort iNADDR_ANY


testSendReqStateless = do
                        readyStack <- initStack (newStack simpleResponder)
                        stackSent <- (uncurry (sendRequest emptyResponseHandler)) (createRequest INVITE testFromURI testToURI readyStack)
                        return ()




testSendReqReceiveResp = do
                          stackA <- initStack (newStack simpleResponder)
                          let uaCtx = createUAContext localAddr stackA
                          let (clientCtx, stackC) = createClientContext remoteAddr INVITE testResponseHandler uaCtx
                          stackD <- sendReq (requestPrototype clientCtx) (clientCtx, stackC)
                          serveOne stackD
  where
     localURI = SipURI False (BS.pack "alice") (BS.empty) (BS.pack "alicehost") (Just (fromIntegral 5060)) []
     localAddr = SipAddress (Just (BS.pack "Alice")) localURI []
     remoteURI = SipURI False (BS.pack "bob") BS.empty (BS.pack "127.0.0.1") (Just (fromIntegral 5060)) []
     remoteAddr = SipAddress (Just (BS.pack "Bob")) remoteURI []

