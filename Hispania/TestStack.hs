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

import Control.Monad.Error

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
simpleResponder req serverCtx = 
          do 
            runErrorT (respond serverCtx response)
            -- ignore both possible error and result, just return unit
            return ()
  where
    response = createResponseTo req 200

printingHandler :: RequestHandler
printingHandler req serverCtx = ioSipAction ( do
                                               System.IO.putStrLn "Received request"
                                               hFlush stdout
                                               return ()
                                            )

uaPrintingHandler :: UARequestHandler
uaPrintingHandler req serverCtx uaCtx = ioSipAction ( do
                                                        System.IO.putStrLn "Received request"
                                                        hFlush stdout
                                                        return ()
                                                     )


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


                     


inject :: BS.ByteString -> TestEnv -> IO Int
inject msg env = sendTo (remoteSock env) msg (localSockAddr env)

handleAll :: TestEnv -> IO TestEnv
handleAll env = serveOne (testedStack env) >>= \x -> return (env{testedStack = (snd x)})


catchOutgoing :: TestEnv -> IO (BS.ByteString, SockAddr)
catchOutgoing env = recvFrom (remoteSock env) 1500

newtype TestAction a = TestAction{runTest:: TestEnv -> IO (a, TestEnv)}

instance Monad TestAction where
  return k = TestAction (\env -> return (k, env))
  p >>= q  = TestAction (\env -> do
                                  (x, envX) <- (runTest p) env
                                  (runTest (q x)) envX
                         )

transform :: (TestEnv -> IO a) -> (TestEnv -> IO (a, TestEnv))
transform f = \env -> do
                       v <- f env
                       return (v, env)




catchOutgoingAction = TestAction (transform catchOutgoing)

injectAction :: BS.ByteString -> TestAction Int
injectAction = TestAction . transform . inject

finishTestAction = TestAction (\env -> (finishTest env >> return ((),env)))

handleAllAction = TestAction (\env -> (handleAll env >>= \newEnv -> return (newEnv,newEnv)))

logAction :: String -> TestAction ()
logAction str = TestAction (\env -> (System.IO.putStrLn str) >> return ((), env))

withStack :: StackAction a -> TestAction a
withStack stackAction = TestAction (\env -> do
                                             (v, stack) <- runStackAction stackAction (testedStack env)
                                             let updatedEnv = env{testedStack=stack}
                                             return (v,updatedEnv)
                                    )

remotePortAction :: TestAction Word16
remotePortAction = TestAction (\env -> return ((getPort (remoteSockAddr env)), env))


-- |-------------------------------------------------------------
-- |
-- | Tests for server functionality
-- |
-- |-------------------------------------------------------------


incomingRegisterStr = "REGISTER sip:user@host SIP/2.0\r\nFrom: OrigUser <sip:originator@domain.dom>\r\nTo: RecepientUser <sip:recepient@domain.dom>\r\nCall-ID: 123321\r\nVia: SIP/2.0/UDP 127.0.0.1:5060;branch=555123123123123\r\n\r\n"
incomingRegister = BS.pack incomingRegisterStr

testIncomingRegister = do
                         testEnv <- prepareTest defaultConfig printingHandler
                         sendCount <- inject message testEnv
                         updatedEnv <- handleAll testEnv
                         finishTest updatedEnv
   where
     message = incomingRegister


testIncomingRegister2 = do
                         testEnv <- prepareTest defaultConfig simpleResponder
                         sendCount <- inject message testEnv
                         updatedEnv <- handleAll testEnv
                         (resp, to) <- catchOutgoing testEnv
                         System.IO.putStrLn (BS.unpack resp)
                         finishTest updatedEnv
   where
     message = incomingRegister

doubleResponder req serverCtx = do
                                  let provisionalResp = createResponseTo req 100
                                  runErrorT (respond serverCtx provisionalResp)
                                  let finalResp = createResponseTo req 200
                                  runErrorT (respond serverCtx finalResp)
                                  return ()


testIncomingRegister3 = do
                         testEnv <- prepareTest defaultConfig doubleResponder
                         runTest ( do
                                     sendCount <- injectAction message
                                     handleAllAction
                                     (resp, to) <- catchOutgoingAction
                                     logAction (BS.unpack resp)
                                     (resp, to) <- catchOutgoingAction 
                                     logAction (BS.unpack resp)
                                     finishTestAction
                                  ) testEnv
   where
     message = incomingRegister




-- |-------------------------------------------------------------
-- |
-- | Tests for client functionality
-- |
-- |-------------------------------------------------------------



emptyResponseHandler :: ResponseHandler
emptyResponseHandler res clientCtxRef = ioSipAction (do
                                                        System.IO.putStrLn "Received response"
                                                        hFlush stdout
                                                        return ()
                                                     )


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





testSendReqReceiveResp = let handler = do
                                         uaCtx <- createUAContext localAddr uaPrintingHandler
                                         clientCtxResult <- runErrorT (createClientContext remoteAddr INVITE emptyResponseHandler uaCtx)
                                         case clientCtxResult of
                                            Left error -> ioSipAction (System.IO.putStrLn error)
                                            Right (clientCtx, requestPrototype) -> sendReq requestPrototype clientCtx
                         in 
                            do 
                              stack <- initStack (newStack printingHandler)
                              (unit, stack) <- runStackAction (inNewSession handler) stack
                              serveOne stack
  where
     localURI = SipURI False (BS.pack "alice") (BS.empty) (BS.pack "alicehost") (Just (fromIntegral 5060)) []
     localAddr = SipAddress (Just (BS.pack "Alice")) localURI []
     remoteURI = SipURI False (BS.pack "bob") BS.empty (BS.pack "127.0.0.1") (Just (fromIntegral 5060)) []
     remoteAddr = SipAddress (Just (BS.pack "Bob")) remoteURI []



testOutgInvite = do
                  testEnv <- prepareTest defaultConfig doubleResponder
                  runTest ( do
                              remotePort <- remotePortAction
                              logAction ("remote port is: " ++ (show remotePort))
                              withStack (inNewSession (sender remotePort))
                              logAction "something sent"
                              (req, to) <- catchOutgoingAction
                              logAction (BS.unpack req)
                              let response = makeResponse req
                              logAction (BS.unpack response)
                              sendCount <- injectAction response
                              handleAllAction
                              logAction "response injected"
                              finishTestAction
                           ) testEnv

   where
     makeResponse req = let reqLines = BS.splitWith (\c -> (c == '\n')) req in
                        let respLines = (BS.pack "SIP/2.0 200 OK\r") : (Prelude.drop 1 reqLines) in
                        intercalate (BS.pack "\n") respLines
     sender remotePort = do
                           uaCtx <- createUAContext localAddr uaPrintingHandler
                           clientCtxResult <- runErrorT (createClientContext (remoteAddr remotePort) INVITE emptyResponseHandler uaCtx)
                           case clientCtxResult of
                              Left error -> ioSipAction (System.IO.putStrLn error)
                              Right (clientCtx, requestPrototype) -> sendReq requestPrototype clientCtx
     localURI = SipURI False (BS.pack "alice") (BS.empty) (BS.pack "alicehost") (Just (fromIntegral 5060)) []
     localAddr = SipAddress (Just (BS.pack "Alice")) localURI []
     remoteURI remotePort = SipURI False (BS.pack "bob") BS.empty (BS.pack "127.0.0.1") (Just remotePort) []
     remoteAddr remotePort = SipAddress (Just (BS.pack "Bob")) (remoteURI remotePort) []




