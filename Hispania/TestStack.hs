module Hispania.TestStack where

import Hispania.Stack
import Hispania.Types
import Hispania.Transport
import Hispania.Parser

import Hispania.TestTypes

import Data.ByteString.Char8 as BS
import Network.BSD
import Network.Socket

import Data.Maybe
import qualified Data.Map as Map

import System.IO.Unsafe

simpleResponder :: RequestHandler
simpleResponder req serverCtx = respond serverCtx response
  where
    response = respondTo req 200


testServer = start (newStack simpleResponder)

testReqURI = (RawURI (BS.pack "sip") (BS.pack "127.0.0.1") )

testOutgoingReq = Request INVITE testReqURI defaultProtoVersion [testFrom, testTo, testCallId] ()

emptyResponseHandler :: ResponseHandler
emptyResponseHandler res = ()


initStack :: Stack -> IO Stack
initStack stack = do
              sock <- socket AF_INET Datagram 0
              bindSocket sock localAddr
              return (updateTransport (addSocketUDP localAddr sock) stack)
   where
     localAddr = SockAddrInet 5657 iNADDR_ANY

start :: Stack -> IO ()
start stack  = do
                 initilaizedStack <- initStack stack
                 serveLoop initilaizedStack


testFromURI = (RawURI (BS.pack "sip") (BS.pack "127.0.0.1") )
testToURI = (RawURI (BS.pack "sip") (BS.pack "127.0.0.1") )

testSendReqStateless = do
                        readyStack <- initStack (newStack simpleResponder)
                        stackSent <- (uncurry (sendRequest emptyResponseHandler)) (createRequest INVITE testFromURI testToURI readyStack)
                        return ()



testResponseHandler resp = unsafePerformIO (Prelude.putStrLn (show resp))

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

